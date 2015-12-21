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

use Pod::Usage;
use Getopt::Long qw(:config auto_version auto_help);
use URI;
use URI::Escape;

# App::GitGerrit was converted from a script into a module following this:
# http://elliotlovesperl.com/2009/11/23/how-to-structure-perl-programs/
use Exporter 'import';
our @EXPORT_OK = qw/run/;

# The $Command variable holds the name of the git-gerrit sub-command that's
# been invoked. It's defined in the 'run' routine below.

our $Command;

# The %Options hash is used to hold the command line options passed to all
# git-gerrit sub-commands. The --debug, --noop, and --help options are common
# to all of them. Each sub-command supports a specific set of options which
# are grokked by the get_options routine below.

my %Options = ( debug => 0, noop => 0, help => 0 );

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

sub get_options {
    my (@opt_specs) = @_;

    # Get defaults from configuration
    foreach my $cmd ($Command, 'all') {
        if (my $options = config("options.$cmd")) {
            debug "$cmd: unshift default options: $options";
            unshift @ARGV, split(' ', $options);
        }
    }

    GetOptions(\%Options, qw/debug noop help/, @opt_specs) or pod2usage(2);
    pod2usage({-exitval => 1, -verbose => 2}) if $Options{help};
}

# The cmd routine is used to invoke shell commands, usually git. It prints out
# the command before invoking it in debug operation.

sub cmd {
    my ($cmd) = @_;
    debug $cmd;
    return 1 if $Options{noop};
    return system($cmd) == 0;
}

# The grok_config routine returns a hash-ref mapping every Git configuration
# variable under the 'git-gerrit' section to its list of values.

sub grok_config {
    state $config;

    unless ($config) {
        debug "git config --list";
        {
            open my $pipe, '-|', 'git config --list';
            while (<$pipe>) {
                if (/^(.+?)\.(\S+)=(.*)/) {
                    push @{$config->{$1}{$2}}, $3;
                } else {
                    info "Strange git-config output: $_";
                }
            }
        }

        # Now we must assume some configuration by default

        $config->{'git-gerrit'}{remote} //= ['origin'];

        my $remote_url = sub {
            state $url;
            unless ($url) {
                my $remote = $config->{'git-gerrit'}{remote}[-1];
                $url = $config->{remote}{"$remote.url"}[-1]
                    or error "The remote '$remote' isn't configured because there's no remote.$remote.url configuration";
                $url = URI->new($url);
            }
            return $url;
        };

        unless ($config->{'git-gerrit'}{baseurl}) {
            my $url = $remote_url->();
            $config->{'git-gerrit'}{baseurl} = [$url->scheme . '://' . $url->authority];
        }
        $config->{'git-gerrit'}{baseurl}[-1] =~ s:/+$::; # strip trailing slashes

        unless ($config->{'git-gerrit'}{project}) {
            my $prefix = URI->new($config->{'git-gerrit'}{baseurl}[-1])->path;
            my $path   = $remote_url->()->path;
            if (length $prefix) {
                $prefix eq substr($path, 0, length($prefix))
                    or error <<EOF;
I can't grok git-gerrit.project because git-gerrit.baseurl's path
doesn't match git-gerrit.remote's path:

* baseurl: 
EOF
                $config->{'git-gerrit'}{project} = [substr($path, length($prefix))];
            } else {
                $config->{'git-gerrit'}{project} = [$path];
            }
        }
        $config->{'git-gerrit'}{project}[-1] =~ s:^/+::; # strip leading slashes
    }

    return $config;
}

# The config routine returns the last value associated with Git's
# git-gerrit.$var configuration variable, as output by the 'git config -l'
# command, or undef if the variable isn't defined.

sub config {
    my ($var) = @_;
    state $config = grok_config;
    return exists $config->{'git-gerrit'}{$var} ? $config->{'git-gerrit'}{$var}[-1] : undef;
}

# The configs routine returns all values associated with Git's git-gerrit.$var
# configuration variable or the empty list if the variable isn't defined.

sub configs {
    my ($var) = @_;
    state $config = grok_config;
    return exists $config->{'git-gerrit'}{$var} ? @{$config->{'git-gerrit'}{$var}}  : ();
}

# The cat_git_dir routine concatenates the GIT_DIR with the list of path names
# passed to it, returning the resulting path in a portable way, using
# File::Spec::catfile to do it.

sub cat_git_dir {
    my @names = @_;
    state $git_dir = qx/git rev-parse --git-dir/;
    chomp $git_dir;
    require File::Spec;
    return File::Spec->catfile($git_dir, @names);
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

    # Try to download and install the hook.
    eval { require LWP::Simple };
    if ($@) {
        info "Cannot install $commit_msg hook because you don't have LWP::Simple installed";
    } else {
        info "Installing $commit_msg hook";
        if (LWP::Simple::is_success(LWP::Simple::getstore(config('baseurl') . "/tools/hooks/commit-msg", $commit_msg))) {
            chmod 0755, $commit_msg;
        }
    }
}

# The credential_* routines below use the git-credential command to get and
# set credentials for git commands and also for Gerrit REST interactions.

sub url_userinfo {
    my ($url) = @_;
    if (my $userinfo = $url->userinfo) {
        return split /:/, $userinfo, 2;
    } else {
        return (undef, undef);
    }
}

sub credential_description_file {
    my ($baseurl, $password) = @_;

    my %credential = (
        protocol => $baseurl->scheme,
        host     => $baseurl->host,
        path     => $baseurl->path,
        password => $password,
    );

    # Try to get the username from the baseurl
    my ($username) = url_userinfo($baseurl);
    $credential{username} = $username if $username;

    require File::Temp;
    my $fh = File::Temp->new();

    while (my ($key, $value) = each %credential) {
        $fh->print("$key=$value\n") if $value;
    }

    $fh->print("\n\n");
    $fh->close();

    return ($fh, $fh->filename);
}

my $git_credential_supported = 1;
sub get_credentials {
    my $baseurl = URI->new(config('baseurl'));
    my ($fh, $credfile) = credential_description_file($baseurl);

    my %credentials;
    debug "Get credentials from git-credential";
    open my $pipe, '-|', "git credential fill <$credfile"
        or error "Can't open pipe to git-credential: $!";
    while (<$pipe>) {
        chomp;
        $credentials{$1} = $2 if /^([^=]+)=(.*)/;
    }
    unless (close $pipe) {
        error "Can't close pipe to git-credential: $!" if $!;

        # If we get here it is because the shell invoked by open
        # above couldn't exec git-credential, which most probably
        # means that we're using a pre-1.8 Git, which doesn't
        # support git-credential yet.
        $git_credential_supported = 0;
    }

    my ($username, $password) = @credentials{qw/username password/};

    unless (defined $username && defined $password) {
        debug "Get credentials from git-gerrit.baseurl";
        ($username, $password) = url_userinfo(config('baseurl'));
    }

    unless (defined $username && defined $password) {
        debug "Get credentials from a .netrc file";
        if (eval {require Net::Netrc}) {
            if (my $mach = Net::Netrc->lookup(URI->new(config('baseurl'))->host, $username)) {
                ($username, $password) = ($mach->login, $mach->password);
            }
        } else {
            debug "Failed to require Net::Netrc";
        }
    }

    unless (defined $username && defined $password) {
        debug "Prompt the user for the credentials";
        if (eval {require Term::Prompt}) {
            $username = Term::Prompt::prompt('x', 'Gerrit username: ', '', $ENV{USER});
            $password = Term::Prompt::prompt('p', 'Gerrit password: ', '');
            print "\n";
        } else {
            debug "Failed to require Term::Prompt";
        }
    }

    defined $username or error "Couldn't get credential's username";
    defined $password or error "Couldn't get credential's password";

    return ($username, $password);
}

sub set_credentials {
    my ($username, $password, $what) = @_;

    return 1 unless $git_credential_supported;

    $what =~ /^(?:approve|reject)$/
        or error "set_credentials \$what argument ($what) must be either 'approve' or 'reject'";

    my $baseurl = URI->new(config('baseurl'));
    my ($fh, $credfile) = credential_description_file($baseurl, $password);

    return system("git credential $what <$credfile") == 0;
}

# The get_message routine returns the message argument to the --message
# option. If the option is not present it invokes the git editor to let the
# user compose a message and returns it.

sub get_message {
    return $Options{message} if exists $Options{message};

    chomp(my $editor = qx/git var GIT_EDITOR/);

    error "Please, read 'git help var' to know how to set up an editor for git messages."
        unless $editor;

    require File::Temp;
    my $tmp = File::Temp->new();
    my $filename = $tmp->filename;

    {
        open my $fh, '>', $filename
            or error "Can't open file for writing ($filename): $!\n";
        print $fh <<'EOF';

# Please enter the review message for this change. Lines starting
# with '#' will be ignored, and an empty message aborts the review.
EOF
        close $fh;
    }

    cmd "$editor $filename"
        or error "Aborting because I couldn't invoke '$editor $filename'.";

    my $message;
    {
        open my $fh, '<', $filename
            or error "Can't open file for reading ($filename): $!\n";
        local $/ = undef;       # slurp mode
        $message = <$fh>;
        close $fh;
    }
    $message =~ s/(?<=\n)#.*?\n//gs; # remove all lines starting with '#'
    return $message;
}

# The gerrit routine keeps a cached Gerrit::REST object to which it relays
# REST calls.

sub gerrit {
    my $method = shift;

    state $gerrit;
    unless ($gerrit) {
        my ($username, $password) = get_credentials;
        require Gerrit::REST;
        $gerrit = Gerrit::REST->new(config('baseurl'), $username, $password);
        eval { $gerrit->GET("/projects/" . uri_escape_utf8(config('project'))) };
        if (my $error = $@) {
            set_credentials($username, $password, 'reject') if $error->{code} == 401;
            die $error;
        } else {
            set_credentials($username, $password, 'approve');
        }
    }

    if ($Options{debug}) {
        my ($endpoint, @args) = @_;
        debug "GERRIT->$method($endpoint)";
        if (@args) {
            require Data::Dumper;
            warn Data::Dumper::Dumper(@args);
        }
    }

    if ($Options{noop} && $method ne 'GET') {
        return 1;
    } else {
        return $gerrit->$method(@_);
    }
}

# The gerrit_or_die  routine relays  its arguments to  the gerrit  routine but
# catches any exception and dies with a formatted message. It should be called
# instead of gerrit whenever the caller doesn't want to treat exceptions.

sub gerrit_or_die {
    my $result = eval { gerrit(@_) };
    die $@->as_text if $@;
    return $result;
}

# The normalize_date routine removes the trailing zeroes from a $date.

sub normalize_date {
    my ($date) = @_;
    $date =~ s/\.0+$//;
    return $date;
}

# The query_changes routine receives a list of strings to query the Gerrit
# server and a list of options to pass to Gerrit's /changes REST end-point. It
# returns an array-ref containing a list of array-refs, each containing a list
# of change descriptions.

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
            error "Couldn't parse offline cache $cache: $@" if $@;
            error "Couldn't do offline cache $cache: $!"    unless defined $changes;
            error "Couldn't run offline cache $cache";
        }
    } else {
        $changes = query_changes(['is:open+AND+(owner:self+OR+reviewer:self)'], ['o=ALL_REVISIONS']);

        require Data::Dumper;
        open my $fh, '>', $cache or error "Can't create $cache: $!\n";
        print $fh Data::Dumper->new([$changes])->Indent(1)->Dump();
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

# The current_branch routine returns the name of the current branch or 'HEAD'
# in a dettached head state.

sub current_branch {
    chomp(my $branch = qx/git rev-parse --abbrev-ref HEAD/);
    return $branch;
}

# The update_branch routine receives a local $branch name and updates it with
# the homonym branch in the Gerrit remote.

sub update_branch {
    my ($branch) = @_;

    my $remote = config('remote');
    cmd "git fetch $remote $branch:$branch";
}

# The change_branch_info routine receives the name of a branch. If it's a
# change-branch, it returns a two-element list containing it's upstream name
# and its id. Otherwise, it returns the empty list.

sub change_branch_info {
    my ($branch) = @_;
    if ($branch =~ m:^change/(?<upstream>.*)/(?<id>[^/]+):) {
        return ($+{upstream}, $+{id});
    }
    return;
}

# The current_change_id routine returns the id of the change branch we're
# currently in. If we're not in a change branch, it returns undef.

sub current_change_id {
    my ($branch, $id) = change_branch_info(current_branch);

    return $id;
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

# This routine receives a branch name (normally the upstream of a
# change-branch) and returns a list of users matching the git-gerrit.reviewers
# specifications. The list returned is guaranteed to have no duplicates.

sub auto_reviewers {
    my ($upstream) = @_;
    my $paths;

    my @reviewers;

  REVIEWERS:
    foreach my $spec (configs('reviewers')) {
        if (my ($users, @conditions) = split ' ', $spec) {
            if (@conditions) {
              CONDITION:
                foreach my $condition (@conditions) {
                    if (my ($what, $op, $match) = ($condition =~ /^(branch|path)([=~])(.+)$/i)) {
                        if ($what eq 'branch') {
                            if ($op eq '=') {
                                next CONDITION if $upstream eq $match;
                            } else {
                                my $regex = eval { qr/$match/ };
                                defined $regex
                                    or info "Warning: skipping git-gerrit.reviewers spec with invalid REGEXP ($match)."
                                        and next REVIEWERS;
                                next CONDITION if $upstream =~ $match;
                            }
                        } else {
                            unless ($paths) {
                                $paths = [qx/git diff --name-only ${upstream}..HEAD/];
                                chomp @$paths;
                            }
                            if ($op eq '=') {
                                foreach my $path (@$paths) {
                                    next CONDITION if $path eq $match;
                                }
                            } else {
                                my $regex = eval { qr/$match/ };
                                defined $regex
                                    or info "Warning: skipping git-gerrit.reviewers spec with invalid REGEXP ($match)."
                                        and next REVIEWERS;
                                foreach my $path (@$paths) {
                                    next CONDITION if $path =~ $regex;
                                }
                            }
                        }
                    } else {
                        info "Warning: skipping git-gerrit.reviewers spec with invalid condition ($condition).";
                    }
                    next REVIEWERS;
                }
            }
            push @reviewers, split(/,/, $users);
        }
    }

    # Use a hash to remove duplicates
    my %reviewers = map {$_ => undef} @reviewers;
    return keys %reviewers;
}

# This routine uses the command 'git show-ref' to grok all local change
# branches. It returns a reference to a hash containing three
# keys. The 'heads' key points to a hash mapping every pushed change branch to
# the SHA-1 it's currently pointing to. The 'locals' key points to a hash
# mapping every not-yet-pushed change branch to the SHA-1 it's currently
# pointing to.

sub git_change_refs {
    # Map all change branches to their respective SHA-1.
    my %refs = (
        locals => {},
        heads  => {},
    );
    foreach (qx/git show-ref --heads/) {
        if (my ($sha1, $name, $id) = m:^([0-9a-f]{40}) refs/heads/(change/.+?)(/[0-9]+)?$:) {
            if (defined $id) {
                $refs{heads}{"$name$id"} = $sha1;
            } else {
                $refs{locals}{$name} = $sha1;
            }
        }
    }
    return \%refs;
}

# This routine receives a list of reference names and returns an array-ref of
# strings in this format: "[*] REF SHA-1 SUBJECT". The '*' mark is only
# present if REF is the HEAD branch.

sub log_refs {
    my (@refs) = @_;

    require Text::Table;
    my $table = Text::Table->new(qw/REF LOG/);

    my $current_branch = current_branch;
    my $format = -t STDOUT
        ? '%C(yellow)%h %Cblue(%<(16,trunc)%an)%Creset %s'
        : '%h (%<(16,trunc)%an) %s';
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

# The select_change_refs routine presents a menu listing all existing change
# branches for the user to select interactively. In list context the
# user can select a list of branches which names are returned as a
# list. In scalar context the user can select a single branch which
# name is returned as a scalar. Already pushed change branches are always
# shown. Not yet pushed change branches are only shown if the string 'locals'
# is passed as one argument.

sub select_change_refs {
    my %opts = map {($_ => undef)} @_;

    eval {require Term::Prompt}
        or error "Failed to require Term::Prompt";

    my $refs = git_change_refs;

    my @refs = keys %{$refs->{heads}};
    push @refs, keys %{$refs->{locals}} if exists $opts{locals};

    return unless @refs;

    @refs = sort @refs;

    my $title = wantarray ? 'Select one or more references' : 'Select one reference';

    my @choices = Term::Prompt::prompt(
        'm',
        {
            prompt                     => 'Number?',
            title                      => $title,
            items                      => log_refs(@refs),
            cols                       => 1,
            accept_multiple_selections => wantarray,
            ignore_empties             => 1,
        },
    );

    return wantarray ? @refs[@choices] : $refs[$choices[0]];
}

# This routine is used by all sub-commands that accept zero or more change
# ids. It returns @ARGV, if non-empty. Otherwise, the user is prompted to
# select a subset of the existing change braches which ids are returned. In
# scalar context the user can select a single change branch, which id is
# returned as a scalar.

sub grok_change_args {
    if (wantarray) {
        return @ARGV ? @ARGV : map { (change_branch_info($_))[1] } select_change_refs;
    } else {
        return @ARGV ? $ARGV[0] : change_branch_info(scalar(select_change_refs))->[1];
    }
}

# This routine returns the result of git-status with suitable options. It's
# useful to check if the working tree is dirty before performing any other git
# command.

sub git_status {
    return qx/git status --porcelain --untracked-files=no/;
}

############################################################
# MAIN

# Each git-gerrit sub-command is implemented by an anonymous routine
# associated with one or more names in the %Commands hash.

my %Commands;

$Commands{new} = sub {
    get_options qw( update onto=s );

    my $topic = shift @ARGV
        or syntax_error "$Command: Missing TOPIC.";

    $topic !~ m:/:
        or error "$Command: the topic name ($topic) should not contain slashes.";

    $topic =~ m:\D:
        or error "$Command: the topic name ($topic) should contain at least one non-digit character.";

    my $branch = shift @ARGV || current_branch;

    if (my ($upstream, $id) = change_branch_info($branch)) {
        # If we're on a change-branch the new change-branch is based on the same upstream
        $branch = $upstream;
    }

    if (my $status = git_status) {
        info "Warning: git-status tells me that your working tree is dirty:\n$status\n";
    }

    if ($Options{update}) {
        update_branch($branch)
            or error "$Command: Non-fast-forward pull. Please, merge or rebase your branch first.";
    }

    my $onto = $Options{onto} || $branch;

    cmd "git checkout -b change/$branch/$topic $onto";

    install_commit_msg_hook;

    return;
};

$Commands{push} = sub {
    $Options{rebase} = '';      # false by default
    get_options qw( prune force+ rebase! draft topic=s submit base=s reviewer=s@ cc=s );

    my $branch = current_branch;

    my ($upstream, $id) = change_branch_info($branch)
        or error "$Command: You aren't in a change branch. I cannot push it.";

    my $is_clean = git_status eq '';

    $is_clean or $Options{force}--
            or error <<EOF;
push: Can't push change because git-status is dirty.
      If this is really what you want to do, please try again with --force.
EOF

    my @commits = qx/git log --decorate=no --first-parent --oneline ${upstream}..HEAD/;
    if (@commits == 0) {
        error "$Command: no changes between $upstream and $branch. Pushing would be pointless.";
    } elsif (@commits > 1) {
        error <<EOF unless $Options{force}--;
push: you have more than one commit that you are about to push.
      The outstanding commits are:

 @commits
      If this is really what you want to do, please try again with --force.
EOF
    }

    # Grok the list of parent commits to see if it's a merge commit.
    my @parents = split / /, qx/git log --pretty='format:%p' -1/;

    # A --noverbose option sets $Options{rebase} to '0'.
    if ($is_clean && (@parents < 2) && ($Options{rebase} || $Options{rebase} eq '' && $id =~ /\D/)) {
        update_branch($upstream)
            or error "$Command: Non-fast-forward pull. Please, merge or rebase your branch first.";
        cmd "git rebase $upstream"
            or error "$Command: please resolve this 'git rebase $upstream' and try again.";
    }

    my $refspec = 'HEAD:refs/' . ($Options{draft} ? 'drafts' : 'for') . "/$upstream";

    my @tags;
    if (my $topic = $Options{topic}) {
        push @tags, "topic=$topic";
    } elsif ($id =~ /\D/) {
        push @tags, "topic=$id";
    }

    my @reviewers = (! $Options{draft} && $id =~ /\D/) ? auto_reviewers($upstream) : ();
    if (my $reviewers = $Options{reviewer}) {
        push @reviewers, split(/,/, join(',', @$reviewers));
    }
    if (@reviewers) {
        push @tags, map("r=$_", @reviewers);
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

    my $remote = config('remote');
    cmd "git push $remote $refspec"
        or error "$Command: Error pushing change.";

    if ($is_clean && $Options{prune}) {
        cmd "git checkout $upstream" and cmd "git branch -D $branch";
    } else {
        chomp(my $sha1 = qx/git rev-list -1 HEAD/);
        my $queries = query_changes(["commit:$sha1"]);
        if (@{$queries->[0]}) {
            my $change = $queries->[0][0];
            cmd "git branch -m $branch change/$change->{branch}/$change->{_number}";
        } else {
            error "$Command: Gerrit didn't find the commit just pushed!!!";
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

        require Text::Table;
        my $table = Text::Table->new("ID\n&num", qw/BRANCH STATUS SUBJECT OWNER CR/);

        foreach my $change (sort {$b->{updated} cmp $a->{updated}} @{$changes->[$i]}) {
            if ($Options{verbose}) {
                if (my $topic = gerrit_or_die(GET => "/changes/$change->{id}/topic")) {
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
            syntax_error "$Command: Invalid change specification: '$ARGV[-1]'";
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
        require Text::Table;
        my $table = Text::Table->new('REVIEWER', map {"$_\n&num"} @labels);

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
            or error "$Command: Change $id belongs to a different project ($change->{project}), not $project";

        my ($revision) = values %{$change->{revisions}};

        my ($url, $ref) = @{$revision->{fetch}{http}}{qw/url ref/};

        $branch = "change/$change->{branch}/$change->{_number}";

        cmd "git fetch --force $url $ref:$branch"
            or error "$Command: Can't fetch $url";

        push @change_branches, $branch;
    }

    return @change_branches;
};

$Commands{list} = $Commands{ls} = sub {
    $Command = 'list';

    get_options;
    my $logs = log_refs(sort
                            map {m@^(?:[0-9a-f]{40}) refs/heads/(.*)@}
                                grep {m@ refs/heads/change/@}
                                    qx/git show-ref --heads/);
    print $_, "\n" foreach @$logs;
    return;
};

$Commands{update} = $Commands{up} = sub {
    $Command = 'update';

    $Options{prune} = 1;        # true by default
    get_options qw( prune! offline );

    # Map all existing change branches to their respective SHA-1.
    my $refs = git_change_refs;

    # Grok every open change having me as owner or reviewer.
    my $changes = my_changes($Options{offline});

    # Refresh all change refs and corresponding upstream branches
    my (%fetch, %upstreams);
    my $current_branch = current_branch;
    foreach my $change (@{$changes->[0]}) {
        my $id = $change->{_number};

        my $upstream = $change->{branch};
        $upstreams{$upstream} = undef;

        my $ref = "change/$upstream/$id";

        # Check the change branch
        if (my $sha = delete $refs->{heads}{$ref}) {
            # The change branch exists already.
            if ($sha eq $change->{current_revision}) {
                info "$Command: branch $ref is up-to-date.";
            } elsif (exists $change->{revisions}{$sha}) {
                unless (cmd "git rev-list --max-count=1 --quiet $change->{current_revision} 2>/dev/null") {
                    my ($url, $ref) = @{$change->{revisions}{$change->{current_revision}}{fetch}{http}}{qw/url ref/};
                    cmd "git fetch $url $ref";
                }
                if ($ref eq $current_branch) {
                    if (git_status eq '') {
                        info "$Command: $ref is the current branch and must be reset to the current patchset.";
                        cmd "git reset --hard $change->{current_revision}";
                    } else {
                        info "$Command: $ref is the current branch and should be reset to the current patchset"
                            . "    but your working tree is dirty, so it will be left unchanged.";
                    }
                } else {
                    info "$Command: branch $ref must be updated.";
                    cmd "git branch -f $ref $change->{current_revision}";
                }
            } else {
                info "$Command: branch $ref has been amended locally so it will be left unchanged.";
            }
        } else {
            # The change branch doesn't exist yet. Let's remember to fetch it.
            info "$Command: $ref must be fetched.";
            $fetch{heads}{$ref} = $change->{revisions}{$change->{current_revision}}{fetch}{http}{ref};
        }
    }

    # We'll update all change branches and upstream branches
    # with a single git-fetch.  This array will contain all refspecs to pass
    # to git fetch below.
    my @refspecs;

    # Grok refspecs for missing change branches
    while (my ($lpath, $rpath) = each %{$fetch{heads}}) {
        push @refspecs, "+$rpath:refs/heads/$lpath"
    }

    # Grok refspecs for upstreams
    my $should_merge;
    foreach my $upstream (keys %upstreams) {
        if ($upstream eq $current_branch) {
            # We can't update the current branch with a git-fetch. So, we'll
            # remember to perform a merge later.
            $should_merge = 1;
            push @refspecs, "$upstream";
        } else {
            push @refspecs, "$upstream:$upstream";
        }
    }

    my $remote = config('remote');
    cmd join(' ', 'git fetch', $remote, @refspecs) if @refspecs;

    # Prune change branches that don't have corresponding open
    # changes in Gerrit.
    if ($Options{prune}) {
        if (! $Options{offline} && keys %{$refs->{heads}}) {
            # We have to make sure none of these change branches were amended
            # locally. So, we query Gerrit for their SHA-1's.
            my $change_branches = query_changes(['(+', join('+OR+', map {"commit:$_"} values %{$refs->{heads}}), '+)']);

            # Collect in @prune the change branches that aren't amended.
            my @prune;
            foreach my $change (@{$change_branches->[0]}) {
                my $ref = "change/$change->{branch}/$change->{_number}";
                if (delete $refs->{heads}{$ref}) {
                    push @prune, $ref;
                }
            }

            cmd join(' ', qw/git branch -D/, @prune) if @prune;

            # Any change branch left in %{$refs->{heads}} must have been
            # amended locally.
            foreach my $branch (keys %{$refs->{heads}}) {
                info "$Command: branch $branch has been amended locally so it will be left unchanged.";
            }
        }
    }

    # Pull the current branch (if it's an upstream branch) at the end because
    # the command can fail and the user should notice the failure.
    cmd "git merge --ff-only $remote/$current_branch" if $should_merge;

    # List all change branches
    {
        local $Command = 'list';
        print "\n";
        $Commands{list}->();
    }

    return;
};

$Commands{prune} = sub {
    get_options qw( offline );

    # Map all change branches to their respective SHA-1.
    my $refs = git_change_refs;

    # Grok every open change having me as owner or reviewer.
    my $changes = my_changes($Options{offline});

    # Remember all change branches that are up-to-date to delete them later.
    foreach my $change (@{$changes->[0]}) {
        my $id = $change->{_number};

        my $ref = "change/$change->{branch}/$id";

        # Check the change branch
        if (exists $refs->{heads}{$ref}) {
            # The change branch exists.
            if ($refs->{heads}{$ref} eq $change->{current_revision}) {
                info "$Command: $ref is up-to-date and will be deleted.";
            } elsif (exists $change->{revisions}{$refs->{heads}{$ref}}) {
                info "$Command: $ref is outdated and will be deleted.";
            } else {
                info "$Command: $ref has been amended locally so we won't delete it.";
                delete $refs->{heads}{$ref};
            }
        }
    }

    # Prune change branches that are up-to-date.
    cmd join(' ', qw/git branch -D/, keys %{$refs->{heads}}) if keys %{$refs->{heads}};

    # FIXME: We're not deleting change branches not associated with open changes yet!

    # List all change branches
    {
        local $Command = 'list';
        print "\n";
        $Commands{list}->();
    }

    return;
};

$Commands{checkout} = $Commands{co} = sub {
    $Command = 'checkout';

    get_options qw( update );

    if ($Options{update}) {
        local $Command = 'update';
        $Commands{update}->();
    };

    my $ref = select_change_refs(qw/locals patchsets/);

    cmd "git checkout $ref" if $ref;

    return;
};

$Commands{merge} = sub {
    get_options qw( update );

    if ($Options{update}) {
        local $Command = 'update';
        $Commands{update}->();
    };

    my @refs = select_change_refs(qw/locals/);

    @refs > 0
        or info "$Command: please select at least one change to merge."
            and return;

    my $merge_branch = 'merge/' . join('+', map {m:/([^/]+)$:} @refs);
    cmd "git checkout -b $merge_branch"
        or error "$Command: merge branch ($merge_branch) creation failed.";

    # Merge all changes
    cmd join(' ', 'git merge --no-ff', @refs);
};

$Commands{upstream} = $Commands{ups} = sub {
    $Command = 'upstream';

    get_options qw( prune );

    my $branch = current_branch;

    if (my ($upstream, $id) = change_branch_info($branch)) {
        if (cmd "git checkout $upstream") {
            if ($Options{prune} && $id =~ /\D/) {
                cmd "git branch -D $branch";
            } else {
                info "Keeping $branch";
            }
        }
    } else {
        error "$Command: You aren't in a change branch. There is no upstream to go to.";
    }

    return;
};

$Commands{'cherry-pick'} = $Commands{pick} = sub {
    $Command = 'cherry-pick';

    get_options qw( edit no-commit );

    my @args;
    push @args, '--edit'      if $Options{edit};
    push @args, '--no-commit' if $Options{'no-commit'};

    @ARGV or syntax_error "$Command: Missing CHANGE.";

    my @change_branches = do {
        local $Command = 'fetch';
        $Commands{fetch}->();
    };

    cmd join(' ', 'git cherry-pick', @args, @change_branches);

    return;
};

$Commands{rebase} = sub {
    get_options qw( tip );

    my ($upstream, $id) = change_branch_info(current_branch)
        or error "$Command: You must be in a change branch to invoke rebase.";

    if ($Options{tip}) {
        cmd "git rebase --onto $upstream HEAD^"
            or error "$Command: please resolve this 'git rebase --onto $upstream HEAD^' and try again.";
    } else {
        cmd "git rebase $upstream"
            or error "$Command: please resolve this 'git rebase $upstream' and try again.";
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
        require Text::Table;
        my %labels = map {$_ => undef} map {keys %{$_->{approvals}}} @$reviewers;
        my @labels = sort keys %labels;
        my $table = Text::Table->new('REVIEWER', map {"$_\n&num"} @labels);
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
            or syntax_error "$Command: Invalid vote ($+{vote}). It must be a single digit optionally prefixed by a [-+] sign.";
    }

    error "$Command: Invalid vote $ARGV[0]." if @ARGV > 1;

    error "$Command: You must specify a message or a vote to review."
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

    # If the user is passing any option we require that it mark where
    # they end with a '--' so that we know where the CHANGEs arguments
    # start.
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

    # Grok the URLs of each change
    my @urls;
    my $baseurl = config('baseurl');
    foreach my $id (grok_change_args) {
        my $change = get_change($id);
        push @urls, "$baseurl/#/c/$change->{_number}";
    }

    cmd join(' ', qw/git web--browse/, @options, @urls);
};

$Commands{config} = sub {
    get_options;
    my $config = grok_config;
    my $git_gerrit = $config->{'git-gerrit'}
        or return;
    require Text::Table;
    my $table = Text::Table->new();
    foreach my $var (sort keys %$git_gerrit) {
        foreach my $value (@{$git_gerrit->{$var}}) {
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
    cmd "git version";
    my $baseurl = config('baseurl'); # die unless configured
    print "Gerrit version ";
    my $version = eval { gerrit(GET => '/config/server/version') };
    $version //= "pre-2.7 (Because it doesn't support the 'Get Version' REST Endpoint.)";
    print "$version\n";
    return;
};

# MAIN

sub run {
    $Command = shift @ARGV
        or syntax_error "Missing command name.";

    exists $Commands{$Command}
        or syntax_error "Invalid command: $Command.";

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

