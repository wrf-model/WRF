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
#      master of the main repository (https://github.com/wrf-model/WRF). Near the end git will print a line of statistics
#      describing what changed, which will look something like this:
#         19 files changed, 27 insertions(+), 27 deletions(-)
#      followed by a few more lines and this final message:
#         Branch master set up to track remote branch master from origin.

# Notes:
# - This is a preliminary version of what will hopefully be a more detailed script in the future. This one only performs fast-forward merges.

use strict;

my $username;
my $go_on = "";

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

# Checkout master, fetch "upstream" commits, and perform a fastforward merge
print "\nStep 3: Checking out master, fetching 'upstream' commits, and performing fastforward merge\n\n";
! system("git", "checkout", "master") or die "Can not checkout master: $!\nWhat on earth did you do??\n";
! system("git", "fetch", "upstream", "master") or die "Can not fetch upstream changes from : $!\nWhat on earth did you do??\n";
! system("git", "merge", "--no-commit", "upstream/master") or die "\nCan not perform fastforward merge from upstream/master: $!\n\nTroubleshooting info:\n\n 1. If you receive a message 'fatal: 'upstream/master' does not point to a commit', your git version may be too old. On yellowstone, try `module load git`\n 2. If you receive a different message, there may be intervening changes; if this is expected, issue the command 'git merge upstream/master'\n";

# Finally, push updated master to the Github copy of your fork:
print "\nStep 4: Pushing updated master to fork\n\n";
! system("git", "push", "-u", "origin", "master") or die "\nCan not push updates to origin/master : $!\n";
