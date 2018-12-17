#!/usr/bin/perl -w

# Script for easily updating your fork of the main WRF repository
#
# Author: Michael Kavulich, September 2016
# No rights reserved, this script may be used, copied, or modified for any purpose
#
# Instructions:
# 1. Clone your fork of the repository (if you already have a local clone of your fork this is optional)
#      git clone https://your_username@github.com/your_username/WRF.git
# 2. Enter the directory of the local clone of your fork
#      cd WRF
# 3. Run this script from within the directory structure of your local clone of your fork
#      ./update_fork.pl
#    You will be asked to enter your Github username: enter it and hit "return".
# 4. If all went well, you should see one of two different messages at the end:
#    - If your fork is already up-to-date, you should see "Already up-to-date."
#    - If your fork is not up-to-date, this script initiates a fast-forward merge to bring your fork up-to-date with the
#      develop of the main repository (https://github.com/wrf-model/WRF). Near the end git will print a line of statistics
#      describing what changed, which will look something like this:
#         19 files changed, 27 insertions(+), 27 deletions(-)
#      followed by a few more lines and this final message:
#         Branch develop set up to track remote branch develop from origin.

# Notes:
# - This is a preliminary version of what will hopefully be a more detailed script in the future. This one only performs fast-forward merges.

use strict;

my $username;
my $go_on = "";

# First off: check if we are on develop, and quit if we are not. We want the branch switch to be transparent to users
my $curr_branch = `git rev-parse --abbrev-ref HEAD`;
chomp $curr_branch;
die "\nERROR ERROR ERROR:\nYou are currently on the branch $curr_branch\n\nThis script must be run from the develop branch.\n\nCheck out the develop branch, then run this script, then check out your working branch $curr_branch when the update is finished\n\n" unless $curr_branch eq "develop";


# Prompt user for their username
print "Please enter your Github username:\n";
   while ($go_on eq "") {
      $go_on = <STDIN>;
      chop($go_on);
      if ($go_on eq "") {
         print "Please enter your Github username:\n";
      } else {
         $username = $go_on;
      }
   }

print "Username = $username\n";
my $main_repo = "https://$username\@github.com/wrf-model/WRF.git";
my $fork = "https://$username\@github.com/$username/WRF.git";

# Set main repository as a remote repository named "upstream", per standard git conventions
print "\nStep 1: Setting main repository as a remote repository named 'upstream'\n\n";
! system("git", "remote", "rm", "upstream") or warn "If you see \"error: Could not remove config section 'remote.upstream'\" this is normal! Don't panic!\n";
! system("git", "remote", "add", "upstream", $main_repo) or die "Can not add main repository '$main_repo' for merging: $!\n";

# Set the "push" url for "upstream" to be the user's fork, to avoid accidentally pushing to the main repository
print "\nStep 2: Setting the 'push' url for 'upstream' to the user's fork, to avoid accidentally pushing to the main repository\n\n";
! system("git", "remote", "set-url", "--push", "upstream", $fork) or die "Can not add set push repository '$fork': $!\n";

# Checkout develop, fetch "upstream" commits, and perform a fastforward merge
print "\nStep 3: Fetching 'upstream' commits, and performing fastforward merge\n\n";
! system("git", "fetch", "upstream", "develop") or die "Can not fetch upstream changes from : $!\nSomething has gone seriously wrong! Perhaps you don't have internet access?\n";
! system("git", "merge", "--ff-only", "upstream/develop") or die "\nCan not perform fastforward merge from upstream/develop: $!\n\nTroubleshooting info:\n\n 1. If you receive a message 'fatal: 'upstream/develop' does not point to a commit', your git version may be too old. On yellowstone, try `module load git`\n 2. If you receive a message' fatal: Not possible to fast-forward, aborting.', you have likely made local changes to the develop branch of your fork. All work should be done on branches of your fork, not the develop!\n";

# Finally, push updated develop to the Github copy of your fork:
print "\nStep 4: Pushing updated develop to fork\n\n";
! system("git", "push", "-u", "origin", "develop") or die "\nCan not push updates to origin/develop : $!\n";
