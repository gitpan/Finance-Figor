use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
              'NAME'           => 'Finance::Figor',
              'VERSION_FROM'   => 'Figor.pm', # finds $VERSION
              'PREREQ_PM'      => {
                                   "Finance::Account::Archive" => "0",
                                   "Mail::Sendmail"            => "0.75",
                                   "POSIX"                     => "0",
                                   "Date::Calc"                => "0",
                                   "GD::Graph::lines"          => "0",
                                   "MIME::Base64"              => "0",
                                  },
              ($] >= 5.005 ?    ## Add these new keywords supported since 5.005
               (ABSTRACT_FROM  => 'Figor.pm', # retrieve abstract from module
                AUTHOR         => 'Jeff Pace <jpace@cpan.org>') : ()),
             );
