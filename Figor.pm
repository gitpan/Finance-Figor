#!/usr/bin/perl -w
# -*- perl -*-

package Finance::Balance::Delta;

no strict;

sub AUTOLOAD { 
    my $self = shift;
    $AUTOLOAD =~ s/.*:://; 
    $self->{$AUTOLOAD};
}

sub new {
    my ($class, %opts) = @_;

    bless {
           account  => defined ($opts{account})  ? $opts{account}  : undef,
           previous => defined ($opts{previous}) ? $opts{previous} : undef,
          }, $class;
}

sub difference {
    my $self = shift;
    defined $self->previous() ? $self->account()->balance() - $self->previous() : undef;
}

sub to_string {
    my $self = shift;
    
    my $str = sprintf "%20s %10.2f ", $self->account->name, $self->account->balance;
        
    if (defined $self->previous) {
        my $diffstr = sprintf "%s%.2f", ($self->diff > 0 ? "+" : ""), $self->diff;
        $str .= sprintf "%10.2f %10s", $self->previous, $diffstr;
    }
    else {
        $str .= sprintf "%10s %10s", "n/a", "n/a";
    }
    $str;
}

sub diff {
    my $self = shift;
    defined $self->previous ? $self->account->balance - $self->previous : undef;
}


package Finance::Figor;

use strict;
use Finance::Account::Archive;
use Mail::Sendmail 0.75;        # need version that supports attachments
use POSIX;
use Date::Calc qw( Today Add_Delta_Days Add_Delta_YM Add_Delta_YMD Date_to_Days );

our $VERSION = '1.0';

our $verbose = 0;

sub logmsg {
    if ($verbose) {
        my @msg = @_;
        chomp @msg;
        my ($pkg,  $fname, $line, undef) = caller(0);
        my (undef, undef,  undef, $sub)  = caller(1);
        printf STDERR "[%50s:%-5d] {%-50s} ", $fname, $line, $sub;
        print STDERR @msg, "\n";
    }
}

sub new {
    my ($class, %opts) = @_;
    
    my $self = {
                archive => Finance::Account::Archive->new(@{ $opts{archive_options} })
               };
    bless $self, $class;
    
    if (defined $opts{bank}) {
        logmsg "getting data from bank ...";
        @{ $self->{accounts} } = $opts{bank}->check_balance(@{ $opts{bank_options} });
    }
    elsif (defined $opts{accounts}) {
        @{ $self->{accounts} } = @{ $opts{accounts} };
    }
    else {
        die("Finance::Figor->new expects either:\n" .
            "    bank - reference to an object that has a check_balance method,\n" .
            "        which returns an array of account objects (with name and balance methods);\n" .
            "    accounts - an array of account objects, with name and balance methods");
    }

    logmsg "accounts: " . $self->{accounts};
    logmsg "accounts: " . join(", ", (@{ $self->{accounts} }));

    $self->{deltas} = ();
    for my $acct (@{ $self->{accounts} }) {
        # my $bal = $acct->balance();
        # printf $format, $acct, $bal;

        logmsg "getting balance of 1 month 5 days to 1 month ago for " . $acct->name;
        my @entries = $self->{archive}->get_entries(name       => $acct->name, 
                                                    lowerbound => '1 month 5 days', 
                                                    upperbound => '1 month',
                                                    order      => 'date desc',
                                                    limit      => 1);

        for (@entries) {
            logmsg sprintf "%-25s | %10s | %10s\n", @{ $_ };
        }

        my $prevbal = $entries[0][1];
        
        logmsg "previous balance: " . (defined $prevbal ? $prevbal : "NONE");
        
        push @{ $self->{deltas} }, Finance::Balance::Delta->new(account => $acct, previous => $prevbal);
    }

    for (@{ $self->{accounts} }) {
        logmsg "account: $_";
        if ($self->{archive}->has_entry(name => $_->name, date => $_->date)) {
            logmsg "entry for " . $_->name . " (" . ($_->date || "no date") . ") exists. updating ...";
            $self->{archive}->update_entry(name => $_->name, newbalance => $_->balance, date => $_->date);
        }
        else {
            logmsg "entry for " . $_->name . " (" . ($_->date || "no date") . ") does not exist. adding ...";
            $self->{archive}->add_entry(name => $_->name, balance => $_->balance, date => $_->date);
        }
    }

    $self;
}

sub DESTROY {
    my $self = shift;
    $self->{archive} = undef;
}

sub summarize {
    my $self = shift;
    
    for my $delta (@{ $self->{deltas} }) {
        print $delta->to_string . "\n";
    }

    $self;
}

sub mail {
    my $self = shift;
    my %opts = @_;
    
    my $msg = "";
    $msg = sprintf "%20s %10s %10s %10s\n", "account name", "current", "previous", "diff";
    for my $delta (@{ $self->{deltas} }) {
        $msg .= $delta->to_string . "\n";
    }

    logmsg "options: " . join(" ", %opts);

    my $pmsg = join(" and ", map { "'$_'" } grep { !defined($opts{$_}) } qw(to from));
    die("error in mail parameters: expecting $pmsg") if $pmsg;

    my %mail = ( 
                To      => $opts{to},
                From    => $opts{from},
                Subject => $opts{subject} || 'Daily Balance'
               );

    if (defined $opts{graph} && $opts{graph}) {
        use MIME::Base64;

        my $boundary = "====" . time() . "====";
        $mail{'content-type'} = "multipart/mixed; boundary=\"$boundary\"";
        
        # gif png jpeg
        my $chart = $self->make_chart(@{ $opts{graph_options} });
        $mail{body} = encode_base64($chart);

        $boundary = '--' . $boundary;
        $mail{body} = <<END_OF_BODY;
$boundary
Content-Type: text/plain; charset=us-ascii;
Content-Transfer-Encoding: 7bit

$msg
$boundary
Content-Type: image/png; name="chart.png"
Content-Transfer-Encoding: base64
Content-Disposition: inline; filename="chart.png"

$mail{body}
$boundary--
END_OF_BODY
  ;
    }
    else {
        $mail{Message} = $msg;
    }

    logmsg "sending mail";
    sendmail(%mail) or die $Mail::Sendmail::error;
    logmsg "mail sent";
    $self;
}

sub make_chart {
    use GD::Graph::lines;

    my $self = shift;
    my %opts = @_;

    logmsg "options:";
    for my $k (sort keys %opts) {
        logmsg "$k => $opts{$k}";
    }

    # two months worth of data:
    my @entries = $self->{archive}->get_entries(lowerbound => "2 month", order => "date, name");

    my @data = ();

    my %bydate = ();
    my %names = ();

    for (@entries) {
        my ($name, $amount, $date) = @{ $_ };
        $bydate{$date}{$name} = $amount;
        $names{$name} = 1;
    }

    my @names = sort keys %names;

    my @today = Today;
    # logmsg "today   : " . join("-", @today) . "\n";

    my $todays = Date_to_Days(Today);

    my $day = 0;
    while (1) {
        ++$day;

        # one month back:
        my @current = Add_Delta_YMD(@today, 0, -1, $day);
        # logmsg "current : " . join("-", @current) . "\n";

        # two months back:
        my @previous = Add_Delta_YM(@current, 0, -1);
        # logmsg "previous: " . join("-", @previous) . "\n";

        my $curdate  = sprintf "%4d-%02d-%02d", @current;
        my $prevdate = sprintf "%4d-%02d-%02d", @previous;
        
        # the x-axis label:
        push @{ $data[0] }, sprintf "%d", $current[2];
        
        # logmsg "associating previous date $prevdate with current date $curdate";
    
        my $index = 1;
        for my $name (@names) {
            push @{ $data[$index]               }, $bydate{$curdate}{$name};
            push @{ $data[$index + 1 + $#names] }, $bydate{$prevdate}{$name};

            $index++;
        }
        
        last if Date_to_Days(@current) == $todays;
    }

    my @account_names = ((map { "$_(c)" } @names), (map { "$_(p)" } @names));

    if ($verbose) {
        # spits out a nicely-formatted table of what we're about to chart
        print "    date ";
        for my $lbl ((map { substr($_, 0, 4) . "(c)" } @names),
                     (map { substr($_, 0, 4) . "(p)" } @names)) {
            printf "%8s ", $lbl;
        }
        print "\n";

        for my $dtidx (0 .. $#{ $data[0] }) {
            printf "%8s ", $data[0][$dtidx];
            for my $idx (1 .. $#data) {
                my $d = $data[$idx][$dtidx];
                if (defined($d)) {
                    printf "%8.2f ", $d;
                }
                else {
                    printf "%8s ", "---";
                }
            }
            print "\n";
        }
    }

    my @linecolors = qw(
                        blue
                        red
                        dgreen
                        purple
                        dblue
                        dbrown
                        dgray
                        dpink
                        dpurple
                        dred
                        dyellow
                        gold
                        marine
                        gray
                        green
                        orange
                        black
                        lblue
                        lbrown
                        lgray
                        lgreen
                        lorange
                        lpurple
                        lred
                        lyellow
                        pink
                        cyan
                        white
                        yellow
                       );
    
    my $width     = $opts{width}  || 600;
    my $height    = $opts{height} || 450;

    my $graph     = GD::Graph::lines->new($width, $height);

    my @colors    = defined $opts{colors} ? @{ $opts{colors} } : @linecolors[0 .. $#account_names / 2];

    # there must be a more elegant way than this:
    my @linetypes = sort map { (1, 3) } 0 .. $#account_names / 2;
    
    # If any data are negative, show the zero line as the x axis, not, for
    # example, the -1000 line.
    my $showzero  = grep { defined($_) && $_ < 0 } map { @{ $data[$_] } } 1 .. $#data;

    logmsg "colors: " . join(", ", @colors) . "\n";
    
    $graph->set( 
                dclrs            => \@colors,
                x_label          => 'Date',
                y_label          => 'Amount',
                title            => 'Daily Balances',
                box_axis         => 0,
                line_width       => 3,
                zero_axis_only   => $showzero,
                x_label_position => 1/2,
                y_label_position => 1/2,
                x_label_skip     => 1 + (scalar @{ $data[0] } / 15),
                transparent      => 0,
                line_types       => \@linetypes
               );

    $graph->set_legend(@account_names);

    # this way, current and previous show up in the same column:
    $graph->set(lg_cols => ((scalar @account_names) / 2));
    
    $graph->plot(\@data);

    my $ext = $graph->export_format;
    $graph->gd->$ext();
}


1;

__END__

=head1 NAME

Finance::Figor - Financial assistant

=head1 SYNOPSIS

  use Finance::Figor;
  use Finance::Bank::SomeBank;

  my $f = Finance::Figor->new(bank => Finance::Bank::SomeBank,
                              bank_options => [
                                               username => "jrandom",
                                               password => "s3cr3t"
                                              ]);
  $f->summarize;
    
  $f->mail(to            => 'jrandom@example.com', 
           from          => 'figor@example.com', 
           subject       => "Account Information - " . scalar(localtime),
           graph         => 1,
           graph_options => [
                             width  => 800,
                             height => 600
                            ]);

=head1 DESCRIPTION

Instantiations of this module assist with automated management of one's
finances, collecting data on a regular basis, comparing current balances to
those of a month previously, and emailing the results to the user.

The usual setup is to run Finance::Figor on a daily basis, which over time
collects enough data to show trends, using a graph to overlay the data from the
current month with that of the previous month. Major financial events, such as
paychecks and rent/mortgage/car payements, tend to occur at the same point each
month, so it is often more useful to look at the "delta" of the current balances
versus those of a month ago, which is a better representation of the amount of
discretionary spending available.

This tight feedback loop allows the user to quickly correct aberrant spending
habits, or to spend more freely, knowing that their available discretionary
spending is above average. It also can help the user -- as in the case of this
author, within one day of it happening -- detect fraudulent usage of one's bank
accounts.

=head2 Methods

=over 4

=item B<new>(... parameters ... )

Fetches the current data via the bank object. This will connect (via the
Finance::Account::Archive module) to a database that contains the historic
financial data, and will be updated with the newly-acquired data.

=over 4

=item B<bank>

Name of a class/module that has a C<check_balance> method, which returns an
array of account objects (with name and balance methods). This is required, if
the B<accounts> parameter is not set.

=item B<accounts>

An array of account objects, with name and balance methods. This parameter
should be defined if the C<bank> parameter is not.

=item B<bank_options>

A reference to an array that is passed to the C<check_balance> method of the
C<bank> module.

=back

=item B<summarize>(... parameters ...)

Displays the differences for the accounts in the form of a table.

=item B<mail>(... parameters ...)

Emails the current statistics. Parameters:

=over 4

=item B<to>

The email address of the recipient. This is required.

=item B<from>

The email address of the sender. This is required.

=item B<subject>

The subject of the message. This defaults to "Daily Balance".

=item B<graph>

Whether to include a graph in the email message. The default value is 0 (false).

=item B<graph_options>

A reference to an array that is passed to the C<make_chart> method.

=back

=item B<make_chart>(... parameters ...)

Emails the current statistics. Valid parameters:

=over 4

=item B<width>

The width of the chart to generate. Default value is 600.

=item B<height>

The height of the chart to generate. Default value is 450.

=item B<colors>

The colors for the lines of the chart. By default, the first five values are
blue, red, dark green, purple, and dark blue, which seem to display best, yet
distinctly, against a white background.

=back

=back

=head2 Class Variables

=over 4

=item B<verbose>

Setting this to a non-zero value results in debugging output.

=back

=head1 CAVEATS

(Verbatim from Finance::Bank::LloydsTSB) This is code for B<online banking>,
and that means B<your money>, and that means B<BE CAREFUL>. You are encouraged,
nay, expected, to audit the source of this module yourself to reassure yourself
that I am not doing anything untoward with your banking data. This software is
useful to me, but is provided under B<NO GUARANTEE>, explicit or implied.

=head1 AUTHOR

Jeff Pace C<jpace@cpan.org>

=head1 COPYRIGHT AND LICENSE

Copyright 2005 by Jeff Pace.

This library is free software; you may redistribute it and/or modify it under
the same terms as Perl itself.

=cut
