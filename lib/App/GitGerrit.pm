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

$App::GitGerrit::VERSION = 'unreleased';
package App::GitGerrit;
# ABSTRACT: A container for functions for the git-gerrit program

use Pod::Usage;
use Getopt::Long qw(:config auto_version auto_help);
use URI;
use URI::Escape;

# App::GitGerrit was converted from a script into a module following this:
# http://elliotlovesperl.com/2009/11/23/how-to-structure-perl-programs/
use Exporter 'import';
our @EXPORT_OK = qw/run/;

# The %Options hash is used to hold the command line options passed to
# all git-gerrit subcommands. The --debug option is common to all of
# them. Each subcommand supports a specific set of options which are
# grokked by the get_options routine below.

my %Options = ( debug => 0, help => 0 );

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
    GetOptions(\%Options, 'debug', 'help', @opt_specs) or pod2usage(2);
    pod2usage({-exitval => 1, -verbose => 2}) if $Options{help};
}

# The cmd routine is used to invoke shell commands, usually git. It
# prints out the command before invoking it in debug operation.

sub cmd {
    my ($cmd) = @_;
    debug $cmd;
    return system($cmd) == 0;
}

# The grok_config routine returns a hash-ref mapping every Git
# configuration variable under the 'git-gerrit' section to its list of
# values.

sub grok_config {
    my %config;

    debug "git config --get-regexp \"^git-gerrit\\.\"";
    {
        open my $pipe, '-|', 'git config --get-regexp "^git-gerrit\."';
        while (<$pipe>) {
            if (/^git-gerrit\.(\S+)\s+(.*)/) {
                push @{$config{$1}}, $2;
            } else {
                info "Strange git-config output: $_";
            }
        }
    }

    # Override option defaults
    for my $opt (qw/verbose/) {
        $Options{$opt} = $config{"default-$opt"}[-1]
            if exists $config{"default-$opt"};
    }

    unless ($config{baseurl} && $config{project} && $config{remote}) {
        info "Missing required configuration:";

        warn <<EOF unless $config{baseurl};

Set your Gerrit server base URL like this. Omit --global if you only
want to configure it for this particular repository.

    git config --global git-gerrit.baseurl "https://your.gerrit.domain"
EOF

        warn <<EOF unless $config{project};

Set the Gerrit project associated with your repository like this.

    git config git-gerrit.project "project/name"
EOF

        warn <<EOF unless $config{remote};

Set the git remote pointing to the Gerrit project like this.

    git config git-gerrit.remote "remote"
EOF

        die "\n\n";
    }

    $config{baseurl}[-1] =~ s:/+$::; # trim trailing slashes from the baseurl
    $config{baseurl}[-1] = URI->new($config{baseurl}[-1]);

    chomp(my $gitdir = qx/git rev-parse --git-dir/);
    push @{$config{gitdir}}, $gitdir;

    return \%config;
}

# The config routine returns the value(s) associated with Git's
# git-gerrit.$var configuration variable. In list context it returns
# the list of all values or the empty list if the variable isn't
# defined. In scalar context it returns the variables last set value
# (as output by the 'git config -l' command) or undef if the variable
# isn't defined.

sub config {
    my ($var) = @_;
    state $config = grok_config;
    if (wantarray) {
        return exists $config->{$var} ? @{$config->{$var}}  : ();
    } else {
        return exists $config->{$var} ? $config->{$var}[-1] : undef;
    }
}

# The install_commit_msg_hook routine is invoked by a few of
# git-gerrit subcommands. It checks if the current repository already
# has a commit-msg hook installed. If not, it tries to download and
# install Gerrit's default commit-msg hook, which inserts Change-Ids
# in commits messages.

sub install_commit_msg_hook {
    require File::Spec;

    # Do nothing if it already exists
    my $commit_msg = File::Spec->catfile(scalar(config('gitdir')), 'hooks', 'commit-msg');
    return if -e $commit_msg;

    # Otherwise, check if we need to mkdir the hooks directory
    my $hooks_dir = File::Spec->catdir(scalar(config('gitdir')), 'hooks');
    mkdir $hooks_dir unless -e $hooks_dir;

    # Try to download and install the hook.
    eval { require LWP::Simple };
    if ($@) {
        info "Cannot install commit_msg hook because you don't have LWP::Simple installed";
    } else {
        info "Installing commit_msg hook";
        if (LWP::Simple::is_success(LWP::Simple::getstore(config('baseurl') . "/tools/hooks/commit-msg", $commit_msg))) {
            chmod 0755, $commit_msg;
        }
    }
}

# The credential_* routines below use the git-credential command to
# get and set credentials for git commands and also for Gerrit REST
# interactions.

sub url_userinfo {
    my ($url) = @_;
    if (my $userinfo = $url->userinfo) {
        return split /:/, $userinfo, 2;
    } else {
        return (undef, undef);
    }
}

sub credential_description_file {
    my ($password) = @_;

    my $baseurl = config('baseurl');

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
    my ($fh, $credfile) = credential_description_file;

    my %credentials;
    debug "Try to get credentials from git-credential";
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
        debug "Try to get credentials from git-gerrit.baseurl";
        ($username, $password) = url_userinfo(scalar(config('baseurl')));
    }

    unless (defined $username && defined $password) {
        debug "Try to get credentials from a .netrc file";
        if (eval {require Net::Netrc}) {
            if (my $mach = Net::Netrc->lookup(config('baseurl')->host, $username)) {
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

    my ($fh, $credfile) = credential_description_file($password);

    return system("git credential $what <$credfile") == 0;
}

# The get_message routine returns the message argument to the
# --message option. If the option is not present it invokes the git
# editor to let the user compose a message and returns it.

sub get_message {
    return $Options{message} if exists $Options{message};

    chomp(my $editor = qx/git var GIT_EDITOR/);

    error "Please, read 'git help var' to know how to set up an editor for git messages."
        unless $editor;

    require File::Temp;
    my $tmp = File::Temp->new();

    require File::Slurp;
    File::Slurp::write_file($tmp->filename, <<'EOF');

# Please enter the review message for this change. Lines starting
# with '#' will be ignored, and an empty message aborts the review.
EOF

    cmd "$editor $tmp"
        or error "Aborting because I couldn't invoke '$editor $tmp'.";

    my $message = File::Slurp::read_file($tmp->filename);

    $message =~ s/(?<=\n)#.*?\n//gs; # remove all lines starting with '#'

    return $message;
}

# The gerrit routine keeps a cached Gerrit::REST object to which it
# relays REST calls.

sub gerrit {
    my $method = shift;

    state $gerrit;
    unless ($gerrit) {
        my ($username, $password) = get_credentials;
        require Gerrit::REST;
        $gerrit = Gerrit::REST->new(config('baseurl')->as_string, $username, $password);
        eval { $gerrit->GET("/projects/" . uri_escape_utf8(config('project'))) };
        if ($@) {
            set_credentials($username, $password, 'reject');
            error $@;
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

    return $gerrit->$method(@_);
}

# The normalize_date routine removes the trailing zeroes from a $date.

sub normalize_date {
    my ($date) = @_;
    $date =~ s/\.0+$//;
    return $date;
}

# The query_changes routine receives a list of strings to query the
# Gerrit server. It returns an array-ref containing a list of
# array-refs, each containing a list of change descriptions.

sub query_changes {
    my @queries = @_;

    return [] unless @queries;

    # If we're inside a git repository, restrict the query to the
    # current project's reviews.
    if (my $project = config('project')) {
        $project = uri_escape_utf8($project);
        @queries = map "q=project:$project+$_", @queries;
    }

    push @queries, "n=$Options{limit}" if $Options{limit};

    push @queries, "o=LABELS";

    my $changes = gerrit(GET => "/changes/?" . join('&', @queries));
    $changes = [$changes] if ref $changes->[0] eq 'HASH';

    return $changes;
}

# The get_change routine returns the description of a change
# identified by $id. An optional boolean second argument ($allrevs)
# tells if the change description should contain a description of all
# patchsets or just the current one.

sub get_change {
    my ($id, $allrevs) = @_;

    my $revs = $allrevs ? 'ALL_REVISIONS' : 'CURRENT_REVISION';
    return (gerrit(GET => "/changes/?q=change:$id&o=$revs"))[0][0];
}

# The current_branch routine returns the name of the current branch or
# 'HEAD' in a dettached head state.

sub current_branch {
    chomp(my $branch = qx/git rev-parse --abbrev-ref HEAD/);
    return $branch;
}

# The update_branch routine receives a local $branch name and updates
# it with the homonym branch in the Gerrit remote.

sub update_branch {
    my ($branch) = @_;

    my $remote = config('remote');
    cmd "git fetch $remote $branch:$branch";
}

# The following change_branch_* routines are used to create, list, and
# grok the local change-branches, i.e., the ones we create locally to
# map Gerrit's changes. Their names have a fixed format like this:
# "change/<upstream>/<id>. <Upstream> is the name of the local branch
# from which this change was derived. <Id> can be either a number,
# meaning the numeric id of a change already in Gerrit, or a
# topic-name, which was created by the "git-gerrit new <topic>"
# command.

sub change_branch_new {
    my ($upstream, $topic) = @_;
    error "The TOPIC cannot contain the slash character (/)."
        if $topic =~ m:/:;
    return "change/$upstream/$topic";
}

sub change_branch_lists {
    chomp(my @branches = map s/^\*?\s+//, qx/git branch --list 'change*'/);
    return @branches;
}

sub change_branch_info {
    my ($branch) = @_;
    if ($branch =~ m:^change/(?<upstream>.*)/(?<id>[^/]+):) {
        return ($+{upstream}, $+{id});
    }
    return;
}

# The current_change routine returns a list of two items: the upstream
# and the id of the change branch we're currently in. If we're not in
# a change branch, it returns the empty list.

sub current_change {
    return change_branch_info(current_branch);
}

# The current_change_id routine returns the id of the change branch
# we're currently in. If we're not in a change branch, it returns
# undef.

sub current_change_id {
    my ($branch, $id) = current_change;

    return $id;
}

# This routine receives the hash-ref mapped to the 'Code-Review' label
# in a change's 'labels' key when it's fetched with the option
# LABELS. For more information, please read:
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
# change-branch) and returns a list of users matching the
# git-gerrit.reviewers specifications. The list returned is guaranteed
# to have no duplicates.

sub auto_reviewers {
    my ($upstream) = @_;
    my $paths;

    my @reviewers;

  REVIEWERS:
    foreach my $spec (config('reviewers')) {
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
                                $paths = [qx/git diff --name-only HEAD ^$upstream/];
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

############################################################
# MAIN

# Each git-gerrit subcommand is implemented by an anonymous routine
# associated with one or more names in the %Commands hash.

my %Commands;

$Commands{new} = sub {
    get_options('update');

    my $topic = shift @ARGV
        or syntax_error "new: Missing TOPIC.";

    $topic !~ m:/:
        or error "new: the topic name ($topic) should not contain slashes.";

    $topic =~ m:\D:
        or error "new: the topic name ($topic) should contain at least one non-digit character.";

    my $branch = shift @ARGV || current_branch;

    if (my ($upstream, $id) = change_branch_info($branch)) {
        error "new: You can't base a new change on a change branch ($branch).";
    }

    my $status = qx/git status --porcelain --untracked-files=no/;

    info "Warning: git-status tells me that your working area is dirty:\n$status\n"
        if length $status;

    if ($Options{update}) {
        update_branch($branch)
            or error "new: Non-fast-forward pull. Please, merge or rebase your branch first.";
    }

    cmd "git checkout -b change/$branch/$topic $branch";

    install_commit_msg_hook;

    return;
};

$Commands{query} = sub {
    get_options(
        'verbose',
        'limit=i',
    );

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

    my $changes = query_changes(@queries);

    my $has_text_table = eval {require Text::Table};

    for (my $i=0; $i < @$changes; ++$i) {
        print "\n[$names[$i]=$queries[$i]]\n";
        next unless @{$changes->[$i]};

        state $headings = [qw/ID STATUS CR UPDATED PROJECT BRANCH OWNER SUBJECT/];
        my ($table, $format);

        if ($has_text_table) {
            $table = Text::Table->new(@$headings);
        } else {
            # Find out the largest owner name
            my $width = length 'OWNER';
            foreach my $change (@{$changes->[$i]}) {
                $width = length $change->{owner}{name}
                    if $width < length $change->{owner}{name};
            }
            $format = "%-5s %-9s %2s %-19s %-20s %-12s %-${width}s %s\n";
            printf $format, @$headings;
        }

        foreach my $change (sort {$b->{updated} cmp $a->{updated}} @{$changes->[$i]}) {
            if ($Options{verbose}) {
                if (my $topic = gerrit(GET => "/changes/$change->{id}/topic")) {
                    $change->{branch} .= " ($topic)";
                }
            }
            my @values = (
                $change->{_number},
                $change->{status},
                code_review($change->{labels}{'Code-Review'}),
                normalize_date($change->{updated}),
                $change->{project},
                $change->{branch},
                $change->{owner}{name},
                $change->{subject},
            );
            if ($has_text_table) {
                $table->add(@values);
            } else {
                printf $format, @values;
            }
        }
        print $table->table() if $has_text_table;
    }
    print "\n";

    return;
};

my %StandardQueries = (
    changes => [
        'Outgoing reviews=is:open+owner:self',
        'Incoming reviews=is:open+reviewer:self+-owner:self',
        'Recently closed=is:closed+owner:self+-age:1mon',
    ],
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
            syntax_error "my: Invalid change specification: '$ARGV[-1]'";
        }
    } else {
        # By default we show 'My Changes'
        push @ARGV, @{$StandardQueries{changes}};
    }

    $Commands{query}();

    return;
};

$Commands{show} = sub {
    get_options();

    my $id = shift @ARGV || current_change_id()
        or syntax_error "show: Missing CHANGE.";

    my $change = gerrit(GET => "/changes/$id/detail");

    print <<EOF;
 Change-Num: $change->{_number}
  Change-Id: $change->{change_id}
    Subject: $change->{subject}
      Owner: $change->{owner}{name}
EOF

    for my $date (qw/created updated/) {
        $change->{$date} = normalize_date($change->{$date})
            if exists $change->{$date};
    }

    for my $key (qw/project branch topic created updated status reviewed mergeable/) {
        printf "%12s %s\n", "\u$key:", $change->{$key}
            if exists $change->{$key};
    }

    print "\n";
    # We want to produce a table in which the first column lists the
    # reviewer names and the other columns have their votes for each
    # label. However, the change object has this information
    # inverted. So, we have to first collect all votes.
    my @labels = sort keys %{$change->{labels}};
    my %reviewers;
    while (my ($label, $info) = each %{$change->{labels}}) {
        foreach my $vote (@{$info->{all}}) {
            $reviewers{$vote->{name}}{$label} = $vote->{value};
        }
    }

    # And now we can output the vote table
    my $table = eval {require Text::Table}
        ? Text::Table->new('REVIEWER', map {"$_\n&num"} @labels)
        : undef;

    printf "%-32s %-s\n", 'REVIEWER', join("\t", @labels)
        unless $table;

    foreach my $name (sort keys %reviewers) {
        my @votes = map {$_ > 0 ? "+$_" : $_} map {defined $_ ? $_ : '0'} @{$reviewers{$name}}{@labels};
        if ($table) {
            $table->add($name, @votes);
        } else {
            printf "%-32s %-s\n", substr($name, 0, 32), join("\t", @votes);
        }
    }
    print $table->table() if $table;

    return;
};

$Commands{config} = sub {
    cmd "git config --get-regexp \"^git-gerrit\\.\"";

    return;
};

$Commands{checkout} = $Commands{co} = sub {
    get_options();

    my $id = shift @ARGV || current_change_id()
        or syntax_error "checkout: Missing CHANGE.";

    my $change = get_change($id);

    my ($revision) = values %{$change->{revisions}};

    my ($url, $ref) = @{$revision->{fetch}{http}}{qw/url ref/};

    my $branch = "change/$change->{branch}/$change->{_number}";

    cmd "git fetch $url $ref:$branch"
        or error "Can't fetch $url";

    cmd "git checkout $branch";

    return;
};

$Commands{upstream} = $Commands{up} = sub {
    get_options(
        'keep',
        'delete',
    );

    my $branch = current_branch;

    if (my ($upstream, $id) = change_branch_info($branch)) {
        if (cmd "git checkout $upstream") {
            if ($Options{keep} || ! $Options{delete} && $id =~ /\D/) {
                info "Keeping $branch";
            } else {
                cmd "git branch -D $branch";
            }
        }
    } else {
        error "upstream: You aren't in a change branch. There is no upstream to go to.";
    }

    return;
};

$Commands{'cherry-pick'} = $Commands{cp} = sub {
    # The 'gerrit cherry-pick' sub-commands passes all of its options,
    # but --debug, to 'git cherry-pick'.
    Getopt::Long::Configure('pass_through');
    get_options();

    # Since we're passing through options, they're left at the start
    # of @ARGV. So, we pop the change-id instead of shifting it.
    my $id = pop @ARGV
        or syntax_error "cherry-pick: Missing CHANGE.";

    # Make sure we haven't popped out an option.
    $id !~ /^-/
        or syntax_error "cherry-pick: Missing CHANGE.";

    my $change = get_change($id);

    my ($revision) = values %{$change->{revisions}};

    my ($url, $ref) = @{$revision->{fetch}{http}}{qw/url ref/};

    cmd "git fetch $url $ref"
        or error "cherry-pick: can't git fetch $url $ref";

    cmd join(' ', 'git cherry-pick', @ARGV, 'FETCH_HEAD');

    return;
};

$Commands{push} = sub {
    $Options{rebase} = '';      # false by default
    get_options(
        'keep',
        'force',
        'rebase!',
        'draft',
        'topic=s',
        'submit',
        'base=s',
        'reviewer=s@',
        'cc=s@'
    );

    qx/git status --porcelain --untracked-files=no/ eq ''
        or error "push: Can't push change because git-status is dirty";

    my $branch = current_branch;

    my ($upstream, $id) = change_branch_info($branch)
        or error "push: You aren't in a change branch. I cannot push it.";

    my @commits = qx/git log --decorate=no --oneline HEAD ^$upstream/;
    if (@commits == 0) {
        error "push: no changes between $upstream and $branch. Pushing would be pointless.";
    } elsif (@commits > 1 && ! $Options{force}) {
        error <<EOF;
push: you have more than one commit that you are about to push.
      The outstanding commits are:

 @commits
      If this is really what you want to do, please try again with --force.
EOF
    }

    # A --noverbose option sets $Options{rebase} to '0'.
    if ($Options{rebase} || $Options{rebase} eq '' && $id =~ /\D/) {
        update_branch($upstream)
            or error "push: Non-fast-forward pull. Please, merge or rebase your branch first.";
        cmd "git rebase $upstream"
            or error "push: please resolve this 'git rebase $upstream' and try again.";
    }

    my $refspec = 'HEAD:refs/' . ($Options{draft} ? 'draft' : 'for') . "/$upstream";

    my @tags;
    if (my $topic = $Options{topic}) {
        push @tags, "topic=$topic";
    } elsif ($id =~ /\D/) {
        push @tags, "topic=$id";
    }

    my @reviewers = auto_reviewers($upstream);
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
        or error "push: Error pushing change.";

    unless ($Options{keep}) {
        cmd "git checkout $upstream" and cmd "git branch -D $branch";
    }

    install_commit_msg_hook;

    return;
};

$Commands{reviewer} = sub {
    get_options(
        'add=s@',
        'confirm',
        'delete=s@',
    );

    my $id = shift @ARGV || current_change_id()
        or syntax_error "reviewer: Missing CHANGE.";

    # First try to make all deletions
    if (my $users = $Options{delete}) {
        foreach my $user (split(/,/, join(',', @$users))) {
            gerrit(DELETE => "/changes/$id/reviewers/$user");
        }
    }

    # Second try to make all additions
    if (my $users = $Options{add}) {
        my $confirm = $Options{confirm} ? 'true' : 'false';
        foreach my $user (split(/,/, join(',', @$users))) {
            gerrit(POST => "/changes/$id/reviewers", { reviewer => $user, confirm => $confirm});
        }
    }

    # Finally, list current reviewers
    my @reviewers = gerrit(GET => "/changes/$id/reviewers");
    print "There are ", scalar(@reviewers), " reviewers currently:\n";
    foreach my $reviewer (@reviewers) {
        print "$reviewer->{name}\t$reviewer->{email}\t";
        foreach my $approval (sort keys %{$reviewer->{approvals}}) {
            print "$approval:$reviewer->{approvals}{$approval}";
        } continue {
            print ", ";
        }
        print "\n";
    }

    return;
};

$Commands{review} = sub {
    get_options(
        'message=s',
        'keep',
    );

    my %review;

    if (my $message = get_message) {
        $review{message} = $message;
    }

    # Set all votes
    while (@ARGV && $ARGV[0] =~ /(?<label>.*)=(?<vote>.*)/) {
        shift @ARGV;
        $review{labels}{$+{label} || 'Code-Review'} = $+{vote};
        $+{vote} =~ /^[+-]?\d$/
            or syntax_error "review: Invalid vote ($+{vote}). It must be a single digit optionally prefixed by a [-+] sign.";
    }

    error "review: Invalid vote $ARGV[0]." if @ARGV > 1;

    error "review: You must specify a message or a vote to review."
        unless keys %review;

    if (my $id = shift @ARGV) {
        gerrit(POST => "/changes/$id/revisions/current/review", \%review);
    } else {
        my $branch = current_branch;

        my ($upstream, $id) = change_branch_info($branch)
            or error "review: Missing CHANGE.";

        gerrit(POST => "/changes/$id/revisions/current/review", \%review);

        unless ($Options{keep}) {
            cmd "git checkout $upstream" and cmd "git branch -D $branch";
        }
    }

    return;
};

$Commands{abandon} = sub {
    get_options(
        'message=s',
        'keep',
    );

    my @args;

    if (my $message = get_message) {
        push @args, { message => $message };
    }

    if (my $id = shift @ARGV) {
        gerrit(POST => "/changes/$id/abandon", @args);
    } else {
        my $branch = current_branch;

        my ($upstream, $id) = change_branch_info($branch)
            or error "abandon: Missing CHANGE.";

        gerrit(POST => "/changes/$id/abandon", @args);

        unless ($Options{keep}) {
            cmd "git checkout $upstream" and cmd "git branch -D $branch";
        }
    }

    return;
};

$Commands{restore} = sub {
    get_options('message=s');

    my $id = shift @ARGV || current_change_id()
        or syntax_error "restore: Missing CHANGE.";

    my @args = ("/changes/$id/restore");

    if (my $message = get_message) {
        push @args, { message => $message };
    }

    gerrit(POST => @args);

    return;
};

$Commands{revert} = sub {
    get_options('message=s');

    my $id = shift @ARGV || current_change_id()
        or syntax_error "revert: Missing CHANGE.";

    my @args = ("/changes/$id/revert");

    if (my $message = get_message) {
        push @args, { message => $message };
    }

    gerrit(POST => @args);

    return;
};

$Commands{submit} = sub {
    get_options(
        'no-wait-for-merge',
        'keep',
    );

    my @args;
    push @args, { wait_for_merge => 'true' } unless $Options{'no-wait-for-merge'};

    if (my $id = shift @ARGV) {
        gerrit(POST => "/changes/$id/submit", @args);
    } else {
        my $branch = current_branch;

        my ($upstream, $id) = change_branch_info($branch)
            or error "submit: Missing CHANGE.";

        gerrit(POST => "/changes/$id/submit", @args);

        unless ($Options{keep}) {
            cmd "git checkout $upstream" and cmd "git branch -D $branch";
        }
    }

    return;
};

$Commands{version} = sub {
    print "Perl version $^V\n";
    print "git-gerrit version $App::GitGerrit::VERSION\n";
    cmd "git version";
    my $baseurl = config('baseurl'); # die unless configured
    my $version = eval { gerrit(GET => '/config/server/version') };
    $version //= "unknown (Certainly pre-2.7, since it doesn't support the 'Get Version' REST Endpoint.)";
    print "Gerrit version $version\n";
    return;
};

# MAIN

sub run {
    my $command = shift @ARGV
        or syntax_error "Missing command name.";

    exists $Commands{$command}
        or syntax_error "Invalid command: $command.";

    $Commands{$command}->();

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

