#!/usr/bin/perl -w
# 
# Script to replace all incorrect "include" statements in WRF trunk. For user-defined files, 
# include statements must use quotes, not brackets, according to the C standard:
#
# INCORRECT
# #include <model_data_order.inc>
#
# CORRECT
# #include "model_data_order.inc"

# OPTIONS:
# use "--search_path" to specify the search pattern for files you want to edit. 
#     - Separate different search patterns with spaces
#     - Be sure to escape wildcard characters with a backslash!
#     - Default is --search_path="../\*/\*.F ../\*/\*/\*.F ../\*/\*/\*/\*.F ../\*/\*/\*/\*/\*.F ../\*/\*.inc ../\*/\*/\*.f90 ../\*/\*/\*/\*.f90"
# use "--dryrun=yes" to do a dry run, where all the output of what would be changed is printed out, but no changes are made.
#
# Created by Michael Kavulich, November 2015
# No rights reserved
#

use strict;
use warnings;
use File::Basename;
use Getopt::Long;

my $dryrun = "no";
my $search_path = "../\*/\*.F ../\*/\*/\*.F ../\*/\*/\*/\*.F ../\*/\*/\*/\*/\*.F ../\*/\*.inc ../\*/\*/\*.f90 ../\*/\*/\*/\*.f90";
GetOptions ('search_path=s' => \$search_path,
            "dryrun:s" => \$dryrun ) or die "\nInvalid option(s) specified, view script comments for help\n";

print "\nSearching for brackets in file(s): $search_path\n\n";

my @source_files=glob("$search_path");

my $found=0;
my $notfound=0;
my $changed=0;
foreach my $filename (@source_files) {
   open (IN, $filename) or die "Cannot open file $filename for read: $!";
   my @lines=<IN>;
   close IN;

   if (grep(/#(\s*)(include|INCLUDE)(\s*)</,@lines)) {
      print "Brackets found in file: $filename\n";
      $found ++;
   } else {
#      print "Brackets NOT found in file: $filename\n";
      $notfound ++;
      next;
   }
 
   open (OUT, ">", $filename) or die "Cannot open file $filename for write: $!";
   foreach my $line (@lines) {
      if ($line =~ /#(\s*)(include|INCLUDE)(\s*)</) {
         print "Found line with brackets: $line\n";
         my @inc_files = split /[<>]/,$line;
         if ($inc_files[1] =~ /<mpi.*/) {
            print "Skipping line that contains 'mpi':\n";
            print "$line\n";
         } else {
            if ( $dryrun =~ /y/i ) {
               my $linetemp = $line;
               $linetemp =~ s/<$inc_files[1]>/\"$inc_files[1]\"/;
               print "Changed line to: $linetemp\n";
            } else {
               $line =~ s/<$inc_files[1]>/\"$inc_files[1]\"/;
               print "Changed line to: $line\n";
            }
            $changed++;
         }
      }
      print OUT $line;
   }  
   close OUT;
}

print "\nBrackets found in $found files.\n";
print "\nBrackets NOT found in $notfound files.\n";
print "\n$changed lines changed\n";
