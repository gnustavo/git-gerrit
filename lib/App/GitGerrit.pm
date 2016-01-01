use utf8;
use 5.010;
use strict;
use warnings;
use locale ':not_characters';

# The following incantation is here to avoid this bug:
# https://rt.perl.org/rt3/Public/Bug/Display.html?id=63402
my $encoding;
BEGIN {
    if ($^O eq 'MSWin32') {
	require Win32;
	my $cp = Win32::GetConsoleCP();
	$encoding = ":encoding(cp$cp)";
    } else {
	$encoding = ':locale';
    }
}
use open ':std', $encoding;

package App::GitGerrit;
# ABSTRACT: Git extension to implement a Gerrit workflow

use Data::Dump;
use Getopt::Long qw(:config auto_version auto_help);
use URI;
use URI::Escape;
use List::Util qw/all first/;

# App::GitGerrit was converted from a script into a module following this:
# http://elliotlovesperl.com/2009/11/23/how-to-structure-perl-programs/
use Exporter 'import';
our @EXPORT_OK = qw/run/;

############################################################
# Global variables

# The $Command variable holds the name of the git-gerrit sub-command that's
# been invoked. It's defined in the 'run' routine below and sometimes
# localized by other routines before invoking other commands.

our $Command;

# The %Options hash is used to hold the command line options passed to all
# git-gerrit sub-commands. The --debug, --noop, and --help options are common
# to all of them. Each sub-command supports a specific set of options which
# are grokked by the get_options routine below.

my %Options = ( debug => 0, noop => 0, help => 0 );

############################################################
# Some modules are required only occasionally and we don't wan't to load
# them unless they're needed, to improve performance. So, the next few
# routines are object factories or routine trampolines that the rest of the
# script uses so that we can concentrate the module requiring here.

# Text::Table factory

sub new_table {
    require Text::Table;
    return Text::Table->new(@_);
}

# File::Temp factory

sub new_file_temp {
    require File::Temp;
    return File::Temp->new();
}

# Gerrit::REST factory

sub new_gerrit_rest {
    require Gerrit::REST;
    return Gerrit::REST->new(@_);
}


# Term::Prompt::prompt trampoline

sub prompt {
    require IO::Prompter;
    return IO::Prompter::prompt(@_);
}

sub menu {
    my ($question, $items) = @_;
    require IO::Prompter;
    return IO::Prompter::prompt(
        $question, "\n",
        (@$items <= 52 ? -single : -number),
        -verbatim,
        -menu => $items,
        '>',
    );
}

# Data::Dumper::Dumper trampoline

sub dumper {
    require Data::Dumper;
    local $Data::Dumper::Indent = 1;
    return Data::Dumper::Dumper(@_);
}

# Pod::Usage::pod2usage trampoline

sub pod2usage {
    require Pod::Usage;
    return Pod::Usage::pod2usage(@_);
}

# File::Spec::catfile trampoline

sub catfile {
    require File::Spec;
    return File::Spec->catfile(@_);
}

# LWP::Simple::getstore trampoline

sub getstore {
    require LWP::Simple;
    return LWP::Simple::is_success(LWP::Simple::getstore(@_));
}

############################################################
# Informational and error helper routines.

sub debug {
    my ($msg) = @_;
    warn 'git-gerrit[DEBUG]: ', $msg, "\n" if $Options{debug};
}

sub info {
    my ($msg) = @_;
    warn 'git-gerrit[INFO]: ', $msg, "\n";
}

sub error {
    my ($msg) = @_;
    die 'git-gerrit[ERROR]: ', $msg, "\n";
}

sub syntax_error {
    my ($msg) = @_;
    pod2usage "git-gerrit[SYNTAX]: $msg\n";
}

############################################################
# Helper routines.

# The get_options routine is invoked by each git-gerrit sub-command to get
# the command line options. Since each sub-command has different options,
# the specification is passed to get_options as they would for the
# Getopt::Long::GetOptions routine. Note, though, that since there are a few
# global options, only the specific sub-command options need to be specified
# for get_options.

sub get_options {
    my (@opt_specs) = @_;

    # Get defaults from configuration
    foreach my $cmd ($Command, 'all') {
        if (my $options = config("options.$cmd")) {
            debug("$cmd: unshift default options: $options");
            unshift @ARGV, split(' ', $options);
        }
    }

    GetOptions(\%Options, qw/debug noop help/, @opt_specs) or pod2usage(2);
    pod2usage({-exitval => 1, -verbose => 2}) if $Options{help};
}

# The cmd routine is used to invoke shell commands, usually git. It prints
# out the command before invoking it in debug operation and returns a
# boolean value telling if the command succeeded.

sub cmd {
    my ($cmd) = @_;
    debug($cmd);
    return 1 if $Options{noop};
    return system($cmd) == 0;
}

# Pipe_from opens a command and returns a list of lines piped from it. The
# lines are chompped already. The command is received as a list of words
# that is passed to open.

sub pipe_from {
    my @cmd = @_;
    my $cmd = join(' ', @cmd);
    debug($cmd);
    open my $pipe, '-|', @cmd or error("can't open command '$cmd': $!");
    chomp(my @lines = <$pipe>);
    close $pipe or error(join("\n", "can't close command '$cmd':", map {"  $_"} @lines));
    return @lines;
}

# Grok_basic_config checks if the basic git-gerrit configuration is set and,
# if not, tries to set it up using heuristics or prompting the user.

sub grok_basic_config {
    my ($config) = @_;

    my $gg = $config->{'git-gerrit'};

    unless (exists $gg->{remote}) {
        $config->{remote} //= {};
        my @remotes = map {/(.*)\.url/} grep {/\.url$/} keys %{$config->{remote}};
        error("Please, configure a Git remote for the Gerrit repository") if @remotes == 0;
        $gg->{remote} = [
            (@remotes == 1)
            # If there's only one remote we assume it's the Gerrit one.
            ? $remotes[0]
            # Otherwise, we ask the user.
            : menu(
                'Which one is the Gerrit remote?',
                [ map {"$_\t$config->{remote}{'$_.url'}"} sort @remotes ],
            )
        ];
        # Configure it so we don't have to ask again.
        cmd("git config --local git-gerrit.remote $gg->{remote}[-1]");
    }

    unless (exists $gg->{baseurl} && exists $gg->{project}) {
        # If either the baseurl or the projet isn't defined we'll try to
        # grok them from the remote's URL.
        my $remote = $gg->{remote}[-1];
        my $url = $config->{remote}{"$remote.url"}[-1]
            or error("Remote '$remote' is missing the remote.$remote.url configuration");
        $url = URI->new($url);

        unless (exists $gg->{baseurl}) {
            my $baseurl = $url->scheme . '://' . $url->authority;
            $baseurl =~ s:/+$::; # strip trailing slashes
            $gg->{baseurl} = [$baseurl];
            # Configure it so we don't have to ask again.
            cmd("git config --local git-gerrit.baseurl $baseurl");
        }

        unless ($gg->{project}) {
            # Unless specified, we assume that the Gerrit project is the
            # path part of the remote's URL.
            my $path = $url->path;
            $path =~ s:/+$::;   # strip trailing slashes
            $gg->{project} = [$path];
            # Configure it so we don't have to ask again.
            cmd("git config --local git-gerrit.project $path");
        }
    }

    return;
}

# The grok_config routine returns a hash-ref mapping every Git configuration
# variable under the 'git-gerrit' section to its list of values.

sub grok_config {
    state $config;

    unless ($config) {
        debug("git config --list");
        foreach (pipe_from(qw/git config --list/)) {
            if (/^(.+?)\.(\S+)=(.*)/) {
                push @{$config->{$1}{$2}}, $3;
            } else {
                info("Strange git-config output: $_");
            }
        }
        grok_basic_config($config);
    }

    return $config;
}

# The config routine returns the last value associated with Git's
# git-gerrit.$var configuration variable, as output by the 'git config -l'
# command, or undef if the variable isn't defined.

sub config {
    my ($var) = @_;
    my $config = grok_config();
    return exists $config->{'git-gerrit'}{$var} ? $config->{'git-gerrit'}{$var}[-1] : undef;
}

# The configs routine returns all values associated with Git's git-gerrit.$var
# configuration variable or the empty list if the variable isn't defined.

sub configs {
    my ($var) = @_;
    my $config = grok_config();
    return exists $config->{'git-gerrit'}{$var} ? @{$config->{'git-gerrit'}{$var}}  : ();
}

# Gerrit_remote returns the remote name associated with the Gerrit remote.

sub gerrit_remote {
    state $remote;
    unless (defined $remote) {
        my $config = grok_config();
        exists $config->{remote} or error "ERROR: There is no remote configured for this repository.";
        my @remotes = map {/(.*)\.url/} grep {/\.url$/} keys %{$config->{remote}};
        if (@remotes == 1) {
            $remote = $remotes[0];
        } else {
            $remote = menu(
                'Which one is the Gerrit remote?',
                [ map {"$_\t$config->{remote}{'$_.url'}"} sort @remotes ],
            );
            # Configure it so we don't have
            cmd("git config --local git-gerrit.remote $remote");
        }
    }
    return $remote;
}

# The cat_git_dir routine concatenates the GIT_DIR with the list of path names
# passed to it, returning the resulting path in a portable way.

sub cat_git_dir {
    my @names = @_;
    state $git_dir = qx/git rev-parse --git-dir/;
    chomp $git_dir;
    return catfile($git_dir, @names);
}

# The install_commit_msg_hook routine is invoked by a few of git-gerrit
# sub-commands. It checks if the current repository already has a commit-msg
# hook installed. If not, it tries to download and install Gerrit's default
# commit-msg hook, which inserts Change-Ids in commits messages.

sub install_commit_msg_hook {
    # Do nothing if it already exists
    my $commit_msg = cat_git_dir('hooks', 'commit-msg');
    return if -e $commit_msg;

    # Otherwise, check if we need to mkdir the hooks directory
    my $hooks_dir = cat_git_dir('hooks');
    mkdir $hooks_dir unless -e $hooks_dir;

    info("Installing $commit_msg hook in the background");
    if (my $pid = fork) {
        # father
        return;
    } else {
        # child
        # Try to download and install the hook.
        eval {
            my $hookurl = config('baseurl') . "/tools/hooks/commit-msg";
            if (getstore($hookurl, $commit_msg)) {
                chmod 0755, $commit_msg;
            } else {
                info("Failed to GET the commit-msg hook from $hookurl");
            }
        };
        info("Cannot load LWP::Simple module") if $@;
    }
}

############################################################
# Credential helper routines.

# The credential_* routines below use the git-credential command to get and
# set credentials for git commands and Gerrit REST interactions.  Read 'git
# help credential' to understand them.

# The credential_description_file routine creates a temporary file
# containing the credential description from Gerrit's baseurl and the
# password.

sub credential_description_file {
    my ($baseurl, $password) = @_;

    my %credential = (
        protocol => $baseurl->scheme,
        host     => $baseurl->host,
        path     => $baseurl->path,
        password => $password,
    );

    # Try to get the username from the baseurl
    if (my $userinfo = $baseurl->userinfo) {
        ($credential{username} = $userinfo) =~ s/:.*//;
    }
    my $tmp = new_file_temp();

    while (my ($key, $value) = each %credential) {
        $tmp->print("$key=$value\n") if $value;
    }

    $tmp->print("\n\n");
    $tmp->close();

    return ($tmp, $tmp->filename);
}

# The get_credentials routine tries several methods to grok the username and
# password needed to interact with Gerrit. The methods are, in order:
# * The git-credential command
# * Credentials passed on Gerrit's baseurl
# * Credentials configured in a ~/.netrc file
# * Prompting the user interactively
# If all methods fail it dies with an error message.

my $git_credential_supported = 1;
sub get_credentials {
    my $baseurl = URI->new(config('baseurl'));
    my ($fh, $credfile) = credential_description_file($baseurl);

    my %credentials;
    debug("Get credentials from git-credential");
    my @lines = eval { pipe_from("git credential fill <$credfile") };
    if (my $error = $@) {
        # If $error happened during the pipe close it means we couldn't exec
        # git-credential, which most probably means that we're using a
        # pre-1.8 Git, which doesn't support git-credential yet.
        $git_credential_supported = 0 if $error =~ /close/;
    } else {
        foreach (@lines) {
            $credentials{$1} = $2 if /^([^=]+)=(.*)/;
        }
    }

    my ($username, $password) = @credentials{qw/username password/};

    unless (defined $username && defined $password) {
        debug("Get credentials from git-gerrit.baseurl");
        if (my $userinfo = $baseurl->userinfo) {
            ($username, $password) = split /:/, $userinfo;
        }
    }

    unless (defined $username && defined $password) {
        debug("Get credentials from a .netrc file");
        if (eval {require Net::Netrc}) {
            if (my $mach = Net::Netrc->lookup(URI->new(config('baseurl'))->host, $username)) {
                ($username, $password) = ($mach->login, $mach->password);
            }
        } else {
            debug("Failed to require Net::Netrc");
        }
    }

    unless (defined $username && defined $password) {
        eval {
            debug("Prompt the user for the credentials");
            $username = prompt('Gerrit username:', -default => $ENV{USER});
            $password = prompt('Gerrit password:', -echo => '*');
            print "\n";
        };
        warn "Failed to require IO::Prompter" if $@;
    }

    defined $username or error("Couldn't get credential's username");
    defined $password or error("Couldn't get credential's password");

    return ($username, $password);
}

sub set_credentials {
    my ($username, $password, $what) = @_;

    return 1 unless $git_credential_supported;

    $what =~ /^(?:approve|reject)$/
        or error("set_credentials \$what argument ($what) must be either 'approve' or 'reject'");

    my $baseurl = URI->new(config('baseurl'));
    my ($fh, $credfile) = credential_description_file($baseurl, $password);

    return system("git credential $what <$credfile") == 0;
}

############################################################

# The get_message routine returns the message argument to the --message
# option, which is supported by a few sub-commands. If the option is not
# present it invokes the git editor to let the user compose a message and
# returns it.

sub get_message {
    return $Options{message} if exists $Options{message};

    chomp(my $editor = qx/git var GIT_EDITOR/);

    error("Please, read 'git help var' to know how to set up an editor for git messages.")
        unless $editor;

    my $tmp = new_file_temp();
    my $filename = $tmp->filename;

    {
        open my $fh, '>', $filename
            or error("Can't open file for writing ($filename): $!\n");
        print $fh <<'EOF';

# Please enter the review message for this change. Lines starting
# with '#' will be ignored, and an empty message aborts the review.
EOF
        close $fh;
    }

    cmd("$editor $filename")
        or error("Aborting because I couldn't invoke '$editor $filename'.");

    my $message;
    {
        open my $fh, '<', $filename
            or error("Can't open file for reading ($filename): $!\n");
        local $/ = undef;       # slurp mode
        $message = <$fh>;
        close $fh;
    }
    $message =~ s/(?<=\n)#.*?\n//gs; # remove all lines starting with '#'
    return $message;
}

############################################################
# Gerrit REST helper routines

# The gerrit routine keeps a cached Gerrit::REST object to which it relays
# REST calls.

sub gerrit {
    my $method = shift;

    state $gerrit;
    state ($username, $password, $authorization);
    unless ($gerrit) {
        ($username, $password) = get_credentials;
        $gerrit = new_gerrit_rest(config('baseurl'), $username, $password);
    }

    if ($Options{debug}) {
        my ($endpoint, @args) = @_;
        debug("GERRIT->$method($endpoint)");
        if (@args) {
            warn dumper(@args);
        }
    }

    return 1 if $Options{noop} && $method ne 'GET';

    my $res = eval { $gerrit->$method(@_) };
    if (my $error = $@) {
        if (! defined $authorization) {
            $authorization = 1;
            set_credentials($username, $password, 'reject') if ref $error && $error->{code} == 401;
        }
        die $error;
    } elsif (! defined $authorization) {
        $authorization = 1;
        set_credentials($username, $password, 'approve');
    }
    return $res;
}

# The gerrit_or_die routine relays its arguments to the gerrit routine but
# catches any exception and dies with a formatted message. It should be
# called instead of gerrit whenever the caller doesn't want to treat
# exceptions.

sub gerrit_or_die {
    my $result = eval { gerrit(@_) };
    if (my $error = $@) {
        if (ref $error) {
            die $error->as_text;
        } else {
            die "$error\n";
        }
    }
    return $result;
}

# The normalize_date routine removes the trailing zeroes from a $date.

sub normalize_date {
    my ($date) = @_;
    $date =~ s/\.0+$//;
    return $date;
}

# The query_changes routine receives a list of strings to query the Gerrit
# server and a list of options to pass to Gerrit's /changes REST
# end-point. It returns an array-ref of array-refs of change descriptions as
# returned by Gerrit.

sub query_changes {
    my ($queries, $opts) = @_;

    return [] unless @$queries;

    $opts //= [];

    # If we're inside a git repository, restrict the query to the
    # current project's reviews.
    if (my $project = config('project')) {
        $project = uri_escape_utf8($project);
        @$queries = map "q=project:$project+$_", @$queries;
    }

    my $changes = gerrit_or_die(GET => "/changes/?" . join('&', @$queries, @$opts));
    $changes = [$changes] if ref $changes->[0] eq 'HASH';

    return $changes;
}

# The my_changes routine queries Gerrit about every open change that is owned
# by me or of which I'm a reviewer. It passes the option ALL_REVISIONS to the
# query to get information about all patchsets of each change. The result is
# cached in a file called $GIT_DIR/git-gerrit.cache. The optional argument
# $offline, if true, reuses the cached information avoiding the interaction
# with Gerrit.

sub my_changes {
    my ($offline) = @_;

    my $changes;

    my $cache = cat_git_dir('git-gerrit.cache');

    if ($offline && -r $cache) {
        unless ($changes = do $cache) {
            error("Couldn't parse offline cache $cache: $@") if $@;
            error("Couldn't do offline cache $cache: $!")    unless defined $changes;
            error("Couldn't run offline cache $cache");
        }
    } else {
        $changes = query_changes(['is:open+AND+(owner:self+OR+reviewer:self)'], ['o=ALL_REVISIONS']);

        open my $fh, '>', $cache or error("Can't create $cache: $!\n");
        $fh->print(dumper([$changes]));
    }

    return $changes;
}

# The get_change routine returns the description of a change identified by
# $id. An optional boolean second argument ($allrevs) tells if the change
# description should contain a description of all patchsets or just the
# current one.

sub get_change {
    my ($id, $allrevs) = @_;

    my $revs = $allrevs ? 'ALL_REVISIONS' : 'CURRENT_REVISION';
    return (gerrit_or_die(GET => "/changes/?q=change:$id&o=$revs"))[0][0];
}

# This routine receives the hash-ref mapped to the 'Code-Review' label in a
# change's 'labels' key when it's fetched with the option LABELS. For more
# information, please read:
# https://gerrit-review.googlesource.com/Documentation/rest-api-changes.html#label-info

sub code_review {
    my ($cr) = @_;
    if (! defined $cr) {
        return '';
    } elsif (exists $cr->{rejected}) {
        return '-2';
    } elsif (exists $cr->{disliked}) {
        return '-1';
    } elsif (exists $cr->{approved}) {
        return '+2';
    } elsif (exists $cr->{recommended}) {
        return '+1';
    } else {
        return '';
    }
}

# The current_branch routine returns the name of the current branch or
# 'HEAD' if we're in a dettached head state.

sub current_branch {
    chomp(my $branch = qx/git rev-parse --abbrev-ref HEAD/);
    return $branch;
}

# The change_branch_info routine receives the name of a reference and
# returns a hash-ref containing information about it. The boolean value
# associated with key 'is_change' tells if the reference is in fact a
# change-branch or a change-tag. All keys are present, but they may be
# undefined:
# * name: the branch name
# * remote: the brach.<name>.remote configuration value
# * upstream: the branch.<name>.merge configuration value
# * is_change: a boolean telling if this is a change-branch or not
# * topic: the branch's topic name
# * id: the branch's legacy numeric change id
# * stem: the branch name without the 'change/' prefix

sub change_branch_info {
    my ($branch) = @_;
    my $remote   = config("branch.$branch.remote");
    my $upstream = config("branch.$branch.merge");
    my %info = (
        name      => $branch,
        remote    => $remote,
        upstream  => $upstream,
        is_change => defined $remote && defined $upstream,
    );
    if ($branch =~ m:change/([^/]+)/(\d+)$:) {
        @info{qw/topic id stem/} = ($1, $2, "$1/$2");
    } elsif ($branch =~ m:change/(\d+)$:) {
        @info{qw/id stem/} = ($1, $1);
    } elsif ($branch =~ m:change/([^/]+)$:) {
        @info{qw/topic stem/} = ($1, $1);
    } else {
        $info{is_change} = 0;
    }
    return \%info;
}

# Git_ls_remote executes the command 'git ls-remote --refs' on the Gerrit
# remote to grok all of its references. It returns a hash-ref mapping
# reference names to their respective SHA1.

sub git_ls_remote {
    my %refs;
    foreach (pipe_from(qw/git ls-remote --refs/, gerrit_remote())) {
        my ($sha1, $name) = split;
        $refs{$name} = $sha1;
    }
    return \%refs;
}

# Change_patchsets receives a hash-ref as returned by git_ls_remote or it
# invokes the routine by itself. Then, form all remote references it groks
# the ones representing changes with names in the form
# refs/changes/NN/ID/PID, where ID is a numeric change-ID and PID is a
# numeric patchset-ID. It returns a hash-ref mapping change-IDs to arrays
# which map patchset-IDs to their corresponding SHA1.

sub change_patchsets {
    my ($refs) = shift || git_ls_remote();
    my %changes;
    while (my ($name, $sha1) = each %$refs) {
        if ($name =~ m:^refs/changes/\d+/(\d+)/(\d+):) {
            my ($id, $patchset) = ($1, $2);
            $changes{$id}[$patchset] = $sha1;
        }
    }
    return \%changes;
}

# This routine uses the command git-for-each-ref to grok information about
# references matching $pattern. It returns a hash-ref mapping reference
# names to hashes describing them.  Each reference hash has the following
# keys:
#
# * is_head: a boolean indicating that this reference is the current HEAD
# * name: the reference short-name
# * sha1: the SHA-1 of the commit the reference points to
# * parents: an array-ref of strings containing the SHA-1 of the commit's parents
# * upstream: the ref's upstream branch name (e.g. origin/master)
# * track: the ref's upstream track information (e.g. [ahead 1, behind 2])
# * is_change: a boolean indicating that this is a change-branch
# * topic: the change-branch's topic name (may be undefined)
# * id: the change-branch's numeric id (may be undefined)
# * upremote: if present, the ref's upstream remote name (e.g. origin)
# * upbranch: if present, the ref's upstream branch name (e.g. master)
# * ahead: if present tells how many commits the reference is ahead of its upstream
# * behind: if present tells how many commits the reference is behind its upstream
# * gone: if present indicates that the ref's upstream has been deleted

sub grok_references {
    my ($pattern) = @_;

    my %refs;

    my @cmd = qw/git for-each-ref --format=%(HEAD):%(refname:short):%(objectname):%(parent):%(upstream:short):%(upstream:track)/;
    push @cmd, $pattern if defined $pattern;

    foreach (pipe_from(@cmd)) {
        my ($head, $name, $sha1, $parents, $upstream, $track) = split /:/;
        my %info = (
            is_head  => $head eq '*',
            name     => $name,
            sha1     => $sha1,
            parents  => [split ' ', $parents],
            upstream => $upstream,
            track    => $track,
        );
        @info{qw/is_change topic id/} = do {
            if ($name =~ m:^change/([^/]+)/(\d+)$:) {
                (1, $1, $2);
            } elsif ($name =~ m:^change/([^/]+)$:) {
                (1, $1, undef);
            } else {
                (0, undef, undef);
            }
        };
        if ($upstream) {
            @info{qw/upremote upbranch/} = split '/', $upstream, 2;
            if ($track) {
                $info{ahead}  = $1 if $track =~ /ahead (\d+)/;
                $info{behind} = $1 if $track =~ /behind (\d+)/;
                $info{gone}   = 1  if $track =~ /gone/;
            }
        }
        $refs{$name} = \%info;
    }

    return \%refs;
}

# This routine receives a branch name (normally the upstream of a
# change-branch) and returns a list of users matching the
# git-gerrit.reviewers specifications. The list returned is guaranteed to
# have no duplicates.

sub auto_reviewers {
    my ($upstream) = @_;
    my $paths;

    my @reviewers;

  SPEC:
    foreach my $spec (configs('reviewers')) {
        if (my ($users, @conditions) = split ' ', $spec) {
            foreach my $condition (@conditions) {
                if (my ($what, $op, $match) = ($condition =~ /^(branch|path)([=~])(.+)$/i)) {
                    if ($what eq 'branch') {
                        if ($op eq '=') {
                            next SPEC if $upstream ne $match;
                        } elsif (my $regex = eval { qr/$match/ }) {
                            next SPEC if $upstream !~ $regex;
                        } else {
                            info("Warning: skipping git-gerrit.reviewers spec with invalid REGEXP ($match).");
                            next SPEC;
                        }
                    } else {
                        unless ($paths) {
                            # Grok all paths changed since HEAD branched from upstream
                            $paths = [qx/git diff --name-only ${upstream}..HEAD/];
                            chomp @$paths;
                        }
                        if ($op eq '=') {
                            next SPEC unless grep {$_ eq $match} @$paths;
                        } elsif (my $regex = eval { qr/$match/ }) {
                            next SPEC unless grep {$_ =~ $regex} @$paths;
                        } else {
                            info("Warning: skipping git-gerrit.reviewers spec with invalid REGEXP ($match).");
                            next SPEC;
                        }
                    }
                } else {
                    info("Warning: skipping git-gerrit.reviewers spec with invalid condition ($condition).");
                }
            }
            push @reviewers, split(/,/, $users);
        }
    }

    # Use a hash to remove duplicates
    my %reviewers = map {$_ => undef} @reviewers;
    return sort keys %reviewers;
}

# Git_show_ref uses the command 'git show-ref' to grok all local
# references. It returns a hash-ref mapping reference names to the SHA-1 of
# the commit they're pointing to. Note that reference names are represented
# in full, i.e., starting with 'refs/'.

sub git_show_ref {
    # Map all references to their respective SHA-1.
    my %refs;
    foreach (qx/git show-ref/) {
        chomp;
        my ($sha1, $name) = split ' ', $_, 2;
        $refs{$name} = $sha1;
    }
    return \%refs;
}

# This routine receives a list of reference names and returns an array-ref of
# strings in this format: "[*] REF SHA-1 SUBJECT". The '*' mark is only
# present if REF is the HEAD branch.

sub log_refs {
    my (@refs) = @_;

    my $current_branch = current_branch;
    my $format = -t STDOUT
        ? '%C(yellow)%h %Cblue(%<(16,trunc)%an)%Creset %s'
        : '%h (%<(16,trunc)%an) %s';
    my $table = new_table(qw/REF LOG/);
    foreach my $ref (@refs) {
        chomp(my $log = qx/git log -1 --pretty=format:'$format' $ref/);
        $table->add(
            $ref eq $current_branch ? "* $ref" : "  $ref",
            $log,
        );
    }
    chomp(my @body = $table->body());
    return \@body;
}

# The select_refs routine receives a list of reference names and presents a
# menu listing them all for the user to select them interactively. In list
# context the user can select a list of references which names are returned
# as a list. In scalar context the user can select a single reference which
# name is returned as a scalar.

sub select_refs {
    my (@refs) = @_;

    return unless @refs;

    my $title = wantarray ? 'Select one or more references' : 'Select one reference';

    my @choices = prompt(
        'm',
        {
            prompt                     => 'Number?',
            title                      => $title,
            items                      => log_refs(sort @refs),
            cols                       => 1,
            accept_multiple_selections => wantarray,
            ignore_empties             => 1,
        },
    );

    return wantarray ? @refs[@choices] : $refs[$choices[0]];
}

sub choose_line_from_command {
    my ($what, $cmd) = @_;

    my @lines = qx/$cmd/
        or error("$Command: there's no $what to select from");

    return menu("Choose one $what:" => \@lines);
}

# This routine is used by all sub-commands that accept zero or more change
# ids. It returns @ARGV, if non-empty. Otherwise, the user is prompted to
# select a subset of the existing change braches which ids are returned. In
# scalar context the user can select a single change branch, which id is
# returned as a scalar.

sub grok_change_args {
    if (@ARGV) {
        return @ARGV;
    } else {
        # Grok all change-branches, keep only the ones already pushed and
        # return the list of their numeric ids.
        return
            map { s:.*/:: }
            grep {m:/\d+$:}
            select_refs(keys %{git_references(m:^refs/heads/change/:)});
    }
}

# This routine returns the result of git-status with suitable options. It's
# useful to check if the working tree is dirty before performing any other git
# command.

sub git_status {
    return qx/git status --porcelain --untracked-files=no/;
}

# Update_changes updates the changes identified by the numeric change IDs
# passed to it. If no IDs are passed, it updates all changes the user is
# interested in. Non-existing change-branches are created. Existing
# change-branches are updated to the latest patchset or removed if the
# change has been merged or abandoned, unless they have been locally
# amended.
#
# The first argument ($branches) is a hash-ref containing the description of
# all change-branches as returned by an invocation of
# grok_references('refs/heads/changes/'). The second argument ($head) is a
# hash-ref describing the change-branch in HEAD, or undef if HEAD isn't a
# change-branch. This routine is used by the sub-commands 'update' and
# 'push'.
#
# The routine respects the settings of the following options: offline,
# prune, and all.

sub update_changes {
    my ($branches, $head, @ids) = @_;

    # Grok all changes that must be updated
    my $changes = do {
        my @terms;
        if (@ids) {
            # The --prune option makes sense only if we're updating
            # everything.
            $Options{prune} = 0;
            @terms = map {"change:$_"} @ids;
        } else {
            @terms = (
                # I'm interested in my own changes and the in the ones I've
                # been invited to review. But only if they're still open.
                '(is:open+AND+(owner:self+OR+reviewer:self))',
                # And also, in any other change associated with an existing
                # change-branch.
                map {"change:$_->{id}"}
                grep {$_->{is_change} && defined $_->{id}}
                values %$branches,
            );
        }
        # I'm only interested in open changes
        query_changes(['is:open+AND+(' . join('+OR+', @terms) . ')'], ['o=ALL_REVISIONS']);
    };

    # Let's iterate through each change and see if we need to updated the
    # local change-branch associated with it. We do this by building a hash
    # mapping the change-branches that need updating to their corresponding
    # remote references, so that we can build the necessary refspecs for a
    # git-fetch command later. We have to treat HEAD specially. We also
    # track the branches that have to be pruned.

    my (%fetch, $head_ref, $is_clean, @prunes);
    foreach my $change (@{$changes->[0]}) {
        my $id = $change->{_number};

        # Construct the change-branch name for the change
        my $ref = 'change/' . (defined $change->{topic} ? "$change->{topic}/" : '') . $id;

        # Check the change branch
        if (my $branch = delete $branches->{$ref}) {
            # The change branch exists already.
            if ($branch->{sha1} eq $change->{current_revision}) {
                info("$Command: $ref is up-to-date");
                push @prunes, $ref
                    if $ref ne $head->{name} && $change->{status} =~ /MERGED|ABANDONED/;
            } elsif (exists $change->{revisions}{$branch->{sha1}}) {
                info("$Command: $ref must be updated to the latest patchset");
                if ($ref eq $head->{name}) {
                    # $ref is the current HEAD so we have to be careful not
                    # to update it if the working tree is dirty.
                    $head->{is_clean} = git_status() eq '';
                    info("    but it is HEAD and your working tree is dirty, so it will be left unchanged.")
                        unless $head->{is_clean};
                    $head->{current_revision} = $change->{current_revision};
                    $head->{ref} = $change->{revisions}{$change->{current_revision}}{fetch}{http}{ref};
                } elsif ($change->{status} =~ /MERGED|ABANDONED/) {
                    push @prunes, $ref;
                } else {
                    $fetch{$ref} = $change->{revisions}{$change->{current_revision}}{fetch}{http}{ref};
                }
            } else {
                info("$Command: $ref was amended locally so it will be left unchanged");
            }
        } else {
            # The change branch doesn't exist yet. Let's remember to fetch it.
            info("$Command: $ref must be fetched");
            $fetch{$ref} = $change->{revisions}{$change->{current_revision}}{fetch}{http}{ref};
        }
    }

    # We'll update all change branches and upstream branches with a single
    # git-fetch. This array will contain all refspecs to pass to git fetch
    # below.
    my @refspecs;

    # Grok refspecs for missing change branches
    while (my ($lpath, $rpath) = each %fetch) {
        push @refspecs, "+$rpath:$lpath"
    }
    push @refspecs, $head->{ref} if exists $head->{ref};

    if (@refspecs) {
        my $remote = gerrit_remote();
        cmd("git fetch $remote '" . join("' '", @refspecs) . "'");
        cmd("git reset --hard $head->{current_revision}")
            if exists $head->{current_revision} && $head->{is_clean};
    }

    # There should be no change-branch left on @$branches because we
    # specifically searched for all of them.

    # Prune change branches that don't have corresponding changes
    cmd(join(' ', 'git branch -D', @prunes))
        if @prunes && ! @ids;

    return;
}


############################################################
# MAIN

# Each git-gerrit sub-command is implemented by an anonymous routine
# associated with one or more names in the %Commands hash.

my %Commands;

$Commands{new} = sub {
    get_options qw( onto=s );

    if (my $status = git_status()) {
        exit 0 unless prompt(
            'Your working area is dirty.',
            $status,
            'Do you really want to checkout a new branch now?',
            -yes,
        );
    }

    my $topic_rx = qr/^[a-z][\w-]*$/;
    my $topic = shift @ARGV || prompt(
        'Enter the topic name:',
        -must => {"match $topic_rx" => $topic_rx},
    );

    # If not specified, make the user choose a remote branch as upstream
    my $upstream = shift @ARGV || do {
        # Choose only among the Gerrit remote's branches.
        my $remote = gerrit_remote();
        my $refs = git_show_ref();
        my @upstreams = sort map {m:^refs/remotes/(.*):} grep {m:^refs/remotes/\Q$remote\E:} keys %$refs;
        if (@upstreams == 1) {
            $upstreams[0];
        } else {
            menu('What upstream do you want to change?' => \@upstreams);
        }
    };

    my $onto = $Options{onto} || $upstream;

    cmd("git checkout -b change/$topic $onto") && cmd("git commit --allow-empty --quiet -mnew");

    install_commit_msg_hook;

    return;
};

$Commands{push} = sub {
    $Options{rebase} = '';      # false by default
    get_options qw( force+ rebase! draft topic=s submit base=s reviewer=s@ cc=s update );

    my $branches = grok_references('refs/heads/change/');

    my $head = first {$_->{is_head}} values %$branches
        or error("$Command: You must be in a change branch to push it.");

    my $is_clean = git_status() eq '';

    exit 0 unless
        $is_clean
        || $Options{force}--
        || prompt('Your working area is dirty. Do you really want to push?', -yes);

    if (! exists $head->{ahead}) {
        error("$Command: $head->{name} hasn't diverged from $head->{upstream}. Pushing would be pointless.");
    } elsif ($head->{ahead} > 1) {
        exit 0 unless
            $Options{force}--
            || prompt("You are about to push $head->{ahead} commits. Are you sure?", -yes);
    }

    # Rebase to upstream if possible.
    if (exists $head->{upstream}
            && $is_clean
            && (@{$head->{parents}} < 2)
            && ($Options{rebase} || ! defined $head->{id})) {
        info("$Command: rebasing to $head->{upstream} before first push");
        # First we update the remote branches
        cmd("git remote update $head->{remote}")
            if $Options{update};
        # Then we rebase
        cmd("git rebase --quiet $head->{upstream}")
            or error("$Command: please resolve this 'git rebase $head->{upstream}' and try again.");
        # And finally we update the HEAD's SHA-1
        $head->{sha1} = qx/git rev-parse HEAD/
            or error("PANIC: failed 'git rev-parse HEAD': $!");
        chomp $head->{sha1};
    }

    my $refspec = 'HEAD:refs/' . ($Options{draft} ? 'drafts' : 'for') . "/$head->{upbranch}";

    my @tags;
    if (my $topic = $Options{topic} || $head->{topic}) {
        push @tags, "topic=$topic";
    }

    {
        # Invite auto-reviewers unless it's a draft or we're pushing a new
        # patchset of an existing change.
        my @reviewers = (! $Options{draft} && ! defined $head->{id})
            ? auto_reviewers($head->{upbranch})
            : ();
        if (my $reviewers = $Options{reviewer}) {
            push @reviewers, split(/,/, join(',', @$reviewers));
        }
        push @tags, map("r=$_", @reviewers) if @reviewers;
    }

    if (my $ccs = $Options{cc}) {
        push @tags, map("cc=$_", split(/,/, join(',', @$ccs)));
    }
    if ($Options{submit}) {
        push @tags, 'submit';
    }
    if (my $base = $Options{base}) {
        push @tags, "base=$base";
    }
    if (@tags) {
        $refspec .= '%';
        $refspec .= join(',', @tags);
    }

    # Now we git-push the $refspec and grok from the commands STDERR the ids
    # of the new or updated changes on Gerrit, which are shown by the
    # ReceiveCommits's formatChangeUrl method in the Gerrit code, in a
    # format like this:

    # remote: New Changes:
    # remote:   https://gerrit.cpqd.com.br/18217 Teste
    # remote:
    # remote: Updated Changes:
    # remote:   https://gerrit.cpqd.com.br/18216 Teste

    # We look for lines beginning with 'remote: http' and grok the URL ids
    # in @changes.

    my @change_ids;
    foreach (pipe_from("git push $head->{upremote} $refspec 2>&1")) {
        push @change_ids, $1 if m@^remote:\s+http.+?/(\d+)\s@;
    }

    if (@change_ids == 1) {
        # If we pushed just one change we know its change-id from the
        # git-push output already.
        if (! defined $head->{id}) {
            # If this was the first time we pushed the change-branch we
            # rename it by tacking the change-id to it.
            my $newname = "$head->{name}/$change_ids[0]";
            info("$Command: renaming the change-branch to $newname");
            cmd("git branch -m $head->{name} $newname");
        }
    } elsif (@change_ids > 1) {
        # If we pushed more than one change we have to update them all.
        info("$Command: updating the change-branches of the pushed changes: " . join(' ', @change_ids));
        update_changes($branches, $head, @change_ids);
        if (! defined $head->{id}) {
            # If this was the first time we pushed the change-branch the
            # updating we did must have created a new change-branch for it
            # with a numeric id. We have to see which one it is, check it
            # out, and remove the current branch.

            # First, let's refresh our knowledge about the change-branches
            # to grok the ones created by the update.
            $branches = grok_references('refs/heads/change/');

            if (my $newhead = first {$_ ne $head->{name} && $branches->{$_} eq $head->{sha1}} keys %$branches) {
                # But we can only checkout another branch if our working
                # area clean.
                if ($is_clean) {
                    info("$Command: checking out the new change-branch $newhead->{name}");
                    cmd("git checkout $newhead->{name} && git branch -D $head->{name}");
                } else {
                    info("$Command: cannot checkout the new change-branch $newhead->{name} because the working area is dirty");
                }
            } else {
                error("$Command: couldn't find the new change-branch for the current change-branch");
            }
        }
    }

    install_commit_msg_hook;

    return;
};

$Commands{query} = sub {
    get_options qw( verbose limit=i );

    my (@names, @queries);
    foreach my $arg (@ARGV) {
        if ($arg =~ /(?<name>.*?)=(?<query>.*)/) {
            push @names,   $+{name};
            push @queries, $+{query};
        } else {
            push @names,   "QUERY";
            push @queries, $arg;
        }
    }

    my @opts = ('o=LABELS');
    push @opts, "n=$Options{limit}" if $Options{limit};

    my $changes = query_changes(\@queries, \@opts);

    for (my $i=0; $i < @$changes; ++$i) {
        print "[$names[$i]=$queries[$i]]\n";
        next unless @{$changes->[$i]};

        my $table = new_table("ID\n&num", qw/BRANCH STATUS SUBJECT OWNER CR/);

        foreach my $change (sort {$b->{updated} cmp $a->{updated}} @{$changes->[$i]}) {
            if ($Options{verbose}) {
                if (my $topic = $change->{topic}) {
                    $change->{branch} .= " ($topic)";
                }
            }
            $table->add(
                $change->{_number},
                $change->{branch},
                $change->{status},
                $change->{subject},
                $change->{owner}{name},
                code_review($change->{labels}{'Code-Review'}),
            );
        }
        print $table->table(), "\n";
    }

    return;
};

my %StandardQueries = (
    changes => [
        'Outgoing reviews=is:open+owner:self',
        'Incoming reviews=is:open+reviewer:self+-owner:self',
    ],
    closed  => ['Recently closed=is:closed+owner:self+-age:1mon'],
    drafts  => ['Drafts=is:draft'],
    watched => ['Watched changes=is:watched+status:open'],
    starred => ['Starred changes=is:starred'],
);
$Commands{my} = sub {
    if (@ARGV) {
        if (exists $StandardQueries{$ARGV[-1]}) {
            splice @ARGV, -1, 1, @{$StandardQueries{$ARGV[-1]}};
        } elsif ($ARGV[-1] =~ /^-/) {
            # By default we show 'My Changes'
            push @ARGV, @{$StandardQueries{changes}};
        } else {
            syntax_error("$Command: Invalid change specification: '$ARGV[-1]'");
        }
    } else {
        # By default we show 'My Changes'
        push @ARGV, @{$StandardQueries{changes}};
    }

    {
        local $Command = 'query';
        $Commands{query}();
    }

    return;
};

$Commands{show} = sub {
    get_options qw( verbose );

    my $parameters = $Options{verbose} ? '?o=MESSAGES' : '';
    foreach my $id (grok_change_args) {
        my $change = gerrit_or_die(GET => "/changes/$id/detail$parameters");

        print <<EOF;
 Change-Num: $change->{_number}
  Change-Id: $change->{change_id}
    Subject: $change->{subject}
      Owner: $change->{owner}{name}
EOF

        foreach my $date (qw/created updated/) {
            $change->{$date} = normalize_date($change->{$date})
                if exists $change->{$date};
        }

        foreach my $key (qw/project branch topic created updated status reviewed mergeable/) {
            printf "%12s %s\n", "\u$key:", $change->{$key}
                if exists $change->{$key};
        }

        print "\n";
        # We want to produce a table in which the first column lists the
        # reviewer names and the other columns have their votes for each
        # label. However, the change object has this information inverted. So,
        # we have to first collect all votes.
        my @labels = sort keys %{$change->{labels}};
        my %reviewers;
        while (my ($label, $info) = each %{$change->{labels}}) {
            foreach my $vote (@{$info->{all}}) {
                $reviewers{$vote->{name}}{$label} = $vote->{value};
            }
        }

        # And now we can output the vote table
        my $table = new_table('REVIEWER', map {"$_\n&num"} @labels);

        foreach my $name (sort keys %reviewers) {
            my @votes = map {$_ > 0 ? "+$_" : $_} map {defined $_ ? $_ : '0'} @{$reviewers{$name}}{@labels};
            $table->add($name, @votes);
        }
        print $table->table();

        if ($Options{verbose}) {
            print "\n";
            foreach my $msg (@{$change->{messages}}) {
                # Indent message by four spaces
                $msg->{message} =~ s/^/    /mg;
                $msg->{date} = normalize_date($msg->{date});
                print "$msg->{date}  $msg->{author}{name}\n$msg->{message}\n\n"
            }
        }

        print '-' x 60, "\n";
    }

    return;
};

$Commands{fetch} = sub {
    get_options;

    my $branch;
    my $project = config('project');
    my @change_branches;
    foreach my $id (grok_change_args) {
        my $change = get_change($id);

        $change->{project} eq $project
            or error("$Command: Change $id belongs to a different project ($change->{project}), not $project");

        my ($revision) = values %{$change->{revisions}};

        my ($url, $ref) = @{$revision->{fetch}{http}}{qw/url ref/};

        $branch = "$change->{branch}/$change->{_number}";

        cmd("git fetch --force $url $ref:$branch")
            or error("$Command: Can't fetch $url");

        push @change_branches, $branch;
    }

    return @change_branches;
};

$Commands{list} = sub {
    get_options;
    cmd('git branch --list -vv');
    return;
};

$Commands{update} = sub {
    $Options{prune} = 1;        # true by default
    get_options qw( all prune! offline );

    my $branches = grok_references('refs/heads/change/');

    my $head = first {$_->{is_head}} values %$branches;

    if ($Options{all}) {
        update_changes($branches, $head);
    } elsif (! $head) {
        error("$Command: you're not in a change-branch");
    } elsif (! defined $head->{id}) {
        error("$Command: you're in a change-branch that hasn't been pushed yet");
    } else {
        # The --prune and the --offline options just make sense with the --all option.
        $Options{prune} = $Options{offline} = 0;
        update_changes($branches, $head, $head->{id});
    }

    # List all change branches
    local $Command = 'list';
    print "\n";
    $Commands{list}->();

    return;
};

$Commands{prune} = sub {
    get_options qw( offline );

    # Map all change branches to their respective SHA-1.
    my $refs = git_references(qr:^refs/heads/:);

    # Grok every open change having me as owner or reviewer.
    my $changes = my_changes($Options{offline});

    # Remember all change branches that are up-to-date to delete them later.
    my @todel;
    foreach my $change (@{$changes->[0]}) {
        my $topic = exists $change->{topic} ? "$change->{topic}/" : '';
        my $id    = $change->{_number};
        my $ref   = "change/$topic$id";

        # Check the change branch
        if (my $sha1 = $refs->{"refs/heads/$ref"}) {
            # The change branch exists.
            if ($sha1 eq $change->{current_revision}) {
                info("$Command: $ref is up-to-date and will be deleted.");
                push @todel, $ref;
            } elsif (exists $change->{revisions}{$sha1}) {
                info("$Command: $ref is outdated and will be deleted.");
                push @todel, $ref;
            } else {
                info("$Command: $ref has been amended locally so we won't delete it.");
            }
        }
    }

    # Prune change branches that are up-to-date.
    cmd(join(' ', qw/git branch -D/, @todel)) if @todel;

    # FIXME: We're not deleting change branches not associated with open changes yet!

    # List all change branches
    {
        local $Command = 'list';
        print "\n";
        $Commands{list}->();
    }

    return;
};

$Commands{checkout} = sub {
    get_options qw( update );

    if ($Options{update}) {
        local $Command = 'update';
        $Commands{update}->();
    };

    if (my $line = menu('What change-branch do you want to checkout?' => [qx:git branch --list -vv 'change/*':])) {
        my ($branch) = split ' ', $line, 2;
        # The current branch is identified by an '*' before its name. So, if
        # $branch eq '*' it means we're already at the current branch and we
        # don't need to check it out.
        cmd("git checkout -q $branch") unless $branch eq '*';
    }

    return;
};

$Commands{merge} = sub {
    get_options qw( update );

    if ($Options{update}) {
        local $Command = 'update';
        $Commands{update}->();
    };

    my @refs = select_refs('refs/heads/');

    @refs > 0
        or info("$Command: please select at least one change to merge.")
            and return;

    my $merge_branch = 'merge/' . join('+', map {m:/([^/]+)$:} @refs);
    cmd("git checkout -b $merge_branch")
        or error("$Command: merge branch ($merge_branch) creation failed.");

    # Merge all changes
    cmd(join(' ', 'git merge --no-ff', @refs));
};

$Commands{'cherry-pick'} = sub {
    get_options qw( edit no-commit );

    my @args;
    push @args, '--edit'      if $Options{edit};
    push @args, '--no-commit' if $Options{'no-commit'};

    @ARGV or syntax_error("$Command: Missing CHANGE.");

    my @change_branches = do {
        local $Command = 'fetch';
        $Commands{fetch}->();
    };

    cmd(join(' ', 'git cherry-pick', @args, @change_branches));

    return;
};

$Commands{rebase} = sub {
    get_options qw( tip );

    my $branch = change_branch_info(current_branch)
        or error("$Command: You must be in a change branch to invoke rebase.");

    my $upstream = $branch->{upstream};

    if ($Options{tip}) {
        cmd("git rebase --onto $upstream HEAD^")
            or error("$Command: please resolve this 'git rebase --onto $upstream HEAD^' and try again.");
    } else {
        cmd("git rebase $upstream")
            or error("$Command: please resolve this 'git rebase $upstream' and try again.");
    }
};

$Commands{reviewer} = sub {
    get_options qw( add=s@ confirm delete=s@ );

    foreach my $id (grok_change_args) {
        # First try to make all deletions
        if (my $users = $Options{delete}) {
            foreach my $user (split(/,/, join(',', @$users))) {
                $user = uri_escape_utf8($user);
                gerrit_or_die(DELETE => "/changes/$id/reviewers/$user");
            }
        }

        # Second try to make all additions
        if (my $users = $Options{add}) {
            my $confirm = $Options{confirm} ? 'true' : 'false';
            foreach my $user (split(/,/, join(',', @$users))) {
                gerrit_or_die(POST => "/changes/$id/reviewers", { reviewer => $user, confirm => $confirm});
            }
        }

        # Finally, list current reviewers
        my $reviewers = gerrit_or_die(GET => "/changes/$id/reviewers");

        print "[$id]\n";
        my %labels = map {$_ => undef} map {keys %{$_->{approvals}}} @$reviewers;
        my @labels = sort keys %labels;
        my $table = new_table('REVIEWER', map {"$_\n&num"} @labels);
        $table->add($_->{name}, @{$_->{approvals}}{@labels})
            foreach sort {$a->{name} cmp $b->{name}} @$reviewers;
        print $table->table(), '-' x 60, "\n";
    }

    return;
};

$Commands{review} = sub {
    get_options qw( message=s );

    my %review;

    if (my $message = get_message) {
        $review{message} = $message;
    }

    # Set all votes
    while (@ARGV && $ARGV[0] =~ /(?<label>.*)=(?<vote>.*)/) {
        shift @ARGV;
        $review{labels}{$+{label} || 'Code-Review'} = $+{vote};
        $+{vote} =~ /^[+-]?\d$/
            or syntax_error("$Command: Invalid vote ($+{vote}). It must be a single digit optionally prefixed by a [-+] sign.");
    }

    error("$Command: Invalid vote $ARGV[0].") if @ARGV > 1;

    error("$Command: You must specify a message or a vote to review.")
        unless keys %review;

    foreach my $id (grok_change_args) {
        gerrit_or_die(POST => "/changes/$id/revisions/current/review", \%review);
    }

    return;
};

$Commands{abandon} = sub {
    get_options qw( message=s );

    my @args;

    if (my $message = get_message) {
        push @args, { message => $message };
    }

    foreach my $id (grok_change_args) {
        gerrit_or_die(POST => "/changes/$id/abandon", @args);
    }

    return;
};

$Commands{restore} = sub {
    get_options qw( message=s );

    my @args;

    if (my $message = get_message) {
        push @args, { message => $message };
    }

    foreach my $id (grok_change_args) {
        gerrit_or_die(POST => "/changes/$id/restore", @args);
    }

    return;
};

$Commands{revert} = sub {
    get_options qw( message=s );

    my @args;

    if (my $message = get_message) {
        push @args, { message => $message };
    }

    foreach my $id (grok_change_args) {
        gerrit_or_die(POST => "/changes/$id/revert", @args);
    }

    return;
};

$Commands{submit} = sub {
    get_options qw( no-wait-for-merge );

    my @args;
    push @args, { wait_for_merge => 'true' } unless $Options{'no-wait-for-merge'};

    foreach my $id (grok_change_args) {
        gerrit_or_die(POST => "/changes/$id/submit", @args);
    }

    return;
};

$Commands{web} = sub {
    # The 'gerrit web' sub-command passes all of its options,
    # but --debug, to 'git web--browse'.
    Getopt::Long::Configure('pass_through');
    get_options;

    # If the user is passing any option we require that she marks where they
    # end with a '--' so that we know where the CHANGEs arguments start.
    my @options;
    for (my $i = 0; $i < @ARGV; ++$i) {
        if ($ARGV[$i] eq '--') {
            # We found a mark. Let's move all the options from @ARGV
            # to @options and get rid of the mark.
            @options = splice @ARGV, 0, $i;
            shift @ARGV;
            last;
        }
    }

    my $head = qx/git rev-parse --abbrev-ref HEAD/
        or error("$Command: command git-rev-parse failed: $!");

    if ($head =~ m:^change/[\w-]+/(\d+):) {
        my $id = $1;
        my $url = config('baseurl') . "/$id";
        push @options, $url;
        cmd(join(' ', qw/git web--browse/, @options));
    } else {
        error("$Command: Please, checkout a change-branch before.");
    }

    return;
};

$Commands{config} = sub {
    get_options;
    my $config = grok_config;
    my $gg = $config->{'git-gerrit'};
    my $table = new_table();
    foreach my $var (sort keys %$gg) {
        foreach my $value (@{$gg->{$var}}) {
            $table->add("git-gerrit.$var", $value);
        }
    }
    print $table->table(), "\n";

    return;
};

$Commands{version} = sub {
    get_options;
    local $| = 1;               # auto-flush
    print "Perl version $^V\n";
    print "git-gerrit version ";
    print defined $App::GitGerrit::VERSION ? $App::GitGerrit::VERSION : 'undefined';
    print "\n";
    cmd("git version");
    my $baseurl = config('baseurl'); # die unless configured
    print "Gerrit version ";
    my $version = eval { gerrit(GET => '/config/server/version') };
    $version //= "pre-2.7 (Because it doesn't support the 'Get Version' REST Endpoint.)";
    print "$version\n";
    return;
};

# MAIN

sub run {
    $Command = shift @ARGV || '';

    # Select among all commands sharing the same prefix
    my @matches =
        grep {substr($Command, 0, length($Command)) eq substr($_, 0, length($Command))}
        keys %Commands;

    if (@matches == 0 && $Command ne '') {
        error("Invalid command '$Command'");
    } elsif (@matches == 1) {
        if ($Command ne $matches[0]) {
            $Command = $matches[0];
            warn "$Command\n";
        }
    } else {
        @matches = keys %Commands unless @matches;
        $Command = menu("What do you want to do?", [sort @matches]);
        if ($Command) {
            warn "$Command\n";
        } else {
            exit 0;
        }
    }

    $Commands{$Command}->();

    return 0;
}

1;
__END__


=head1 SYNOPSIS

    use App::GitGerrit qw/run/;
    return 1 if caller;
    exit run();

=head1 DESCRIPTION

You're not supposed to use this module directly!

It's used by the C<git-gerrit> script which comes in the same CPAN
distribution. All the documentation that exists can be read via

    perldoc git-gerrit

