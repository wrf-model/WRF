"""Class for interacting with svn repositories
"""

from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function

import os
import re
import xml.etree.ElementTree as ET

from .global_constants import EMPTY_STR, VERBOSITY_VERBOSE
from .repository import Repository
from .externals_status import ExternalStatus
from .utils import fatal_error, indent_string, printlog
from .utils import execute_subprocess


class SvnRepository(Repository):
    """
    Class to represent and operate on a repository description.

    For testing purpose, all system calls to svn should:

    * be isolated in separate functions with no application logic
      * of the form:
         - cmd = ['svn', ...]
         - value = execute_subprocess(cmd, output_to_caller={T|F},
                                      status_to_caller={T|F})
         - return value
      * be static methods (not rely on self)
      * name as _svn_subcommand_args(user_args)

    This convention allows easy unit testing of the repository logic
    by mocking the specific calls to return predefined results.

    """
    RE_URLLINE = re.compile(r'^URL:')

    def __init__(self, component_name, repo, ignore_ancestry=False):
        """
        Parse repo (a <repo> XML element).
        """
        Repository.__init__(self, component_name, repo)
        self._ignore_ancestry = ignore_ancestry
        if self._url.endswith('/'):
            # there is already a '/' separator in the URL; no need to add another
            url_sep = ''
        else:
            url_sep = '/'
        if self._branch:
            self._url = self._url + url_sep + self._branch
        elif self._tag:
            self._url = self._url + url_sep + self._tag
        else:
            msg = "DEV_ERROR in svn repository. Shouldn't be here!"
            fatal_error(msg)

    # ----------------------------------------------------------------
    #
    # Public API, defined by Repository
    #
    # ----------------------------------------------------------------
    def checkout(self, base_dir_path, repo_dir_name, verbosity, recursive):  # pylint: disable=unused-argument
        """Checkout or update the working copy

        If the repo destination directory exists, switch the sandbox to
        match the externals description.

        If the repo destination directory does not exist, checkout the
        correct branch or tag.
        NB: <recursive> is include as an argument for compatibility with
            git functionality (repository_git.py)

        """
        repo_dir_path = os.path.join(base_dir_path, repo_dir_name)
        if 'github.com' in self._url:
            msg = "SVN access to github.com is no longer supported"
            fatal_error(msg)
        if os.path.exists(repo_dir_path):
            cwd = os.getcwd()
            os.chdir(repo_dir_path)
            self._svn_switch(self._url, self._ignore_ancestry, verbosity)
            # svn switch can lead to a conflict state, but it gives a
            # return code of 0. So now we need to make sure that we're
            # in a clean (non-conflict) state.
            self._abort_if_dirty(repo_dir_path,
                                 "Expected clean state following switch")
            os.chdir(cwd)
        else:
            self._svn_checkout(self._url, repo_dir_path, verbosity)

    def status(self, stat, repo_dir_path):
        """
        Check and report the status of the repository
        """
        self._check_sync(stat, repo_dir_path)
        if os.path.exists(repo_dir_path):
            self._status_summary(stat, repo_dir_path)

    # ----------------------------------------------------------------
    #
    # Internal work functions
    #
    # ----------------------------------------------------------------
    def _check_sync(self, stat, repo_dir_path):
        """Check to see if repository directory exists and is at the expected
        url.  Return: status object

        """
        if not os.path.exists(repo_dir_path):
            # NOTE(bja, 2017-10) this state should have been handled by
            # the source object and we never get here!
            stat.sync_state = ExternalStatus.STATUS_ERROR
        else:
            svn_output = self._svn_info(repo_dir_path)
            if not svn_output:
                # directory exists, but info returned nothing. .svn
                # directory removed or incomplete checkout?
                stat.sync_state = ExternalStatus.UNKNOWN
            else:
                stat.sync_state, stat.current_version = \
                    self._check_url(svn_output, self._url)
            stat.expected_version = '/'.join(self._url.split('/')[3:])

    def _abort_if_dirty(self, repo_dir_path, message):
        """Check if the repo is in a dirty state; if so, abort with a
        helpful message.

        """

        stat = ExternalStatus()
        self._status_summary(stat, repo_dir_path)
        if stat.clean_state != ExternalStatus.STATUS_OK:
            status = self._svn_status_verbose(repo_dir_path)
            status = indent_string(status, 4)
            errmsg = """In directory
    {cwd}

svn status now shows:
{status}

ERROR: {message}

One possible cause of this problem is that there may have been untracked
files in your working directory that had the same name as tracked files
in the new revision.

To recover: Clean up the above directory (resolving conflicts, etc.),
then rerun checkout_externals.
""".format(cwd=repo_dir_path, message=message, status=status)

            fatal_error(errmsg)

    @staticmethod
    def _check_url(svn_output, expected_url):
        """Determine the svn url from svn info output and return whether it
        matches the expected value.

        """
        url = None
        for line in svn_output.splitlines():
            if SvnRepository.RE_URLLINE.match(line):
                url = line.split(': ')[1].strip()
                break
        if not url:
            status = ExternalStatus.UNKNOWN
        elif url == expected_url:
            status = ExternalStatus.STATUS_OK
        else:
            status = ExternalStatus.MODEL_MODIFIED

        if url:
            current_version = '/'.join(url.split('/')[3:])
        else:
            current_version = EMPTY_STR

        return status, current_version

    def _status_summary(self, stat, repo_dir_path):
        """Report whether the svn repository is in-sync with the model
        description and whether the sandbox is clean or dirty.

        """
        svn_output = self._svn_status_xml(repo_dir_path)
        is_dirty = self.xml_status_is_dirty(svn_output)
        if is_dirty:
            stat.clean_state = ExternalStatus.DIRTY
        else:
            stat.clean_state = ExternalStatus.STATUS_OK

        # Now save the verbose status output incase the user wants to
        # see it.
        stat.status_output = self._svn_status_verbose(repo_dir_path)

    @staticmethod
    def xml_status_is_dirty(svn_output):
        """Parse svn status xml output and determine if the working copy is
        clean or dirty. Dirty is defined as:

        * modified files
        * added files
        * deleted files
        * missing files

        Unversioned files do not affect the clean/dirty status.

        'external' is also an acceptable state

        """
        # pylint: disable=invalid-name
        SVN_EXTERNAL = 'external'
        SVN_UNVERSIONED = 'unversioned'
        # pylint: enable=invalid-name

        is_dirty = False
        try:
            xml_status = ET.fromstring(svn_output)
        except BaseException:
            fatal_error(
                "SVN returned invalid XML message {}".format(svn_output))
        xml_target = xml_status.find('./target')
        entries = xml_target.findall('./entry')
        for entry in entries:
            status = entry.find('./wc-status')
            item = status.get('item')
            if item == SVN_EXTERNAL:
                continue
            if item == SVN_UNVERSIONED:
                continue
            is_dirty = True
            break
        return is_dirty

    # ----------------------------------------------------------------
    #
    # system call to svn for information gathering
    #
    # ----------------------------------------------------------------
    @staticmethod
    def _svn_info(repo_dir_path):
        """Return results of svn info command
        """
        cmd = ['svn', 'info', repo_dir_path]
        output = execute_subprocess(cmd, output_to_caller=True)
        return output

    @staticmethod
    def _svn_status_verbose(repo_dir_path):
        """capture the full svn status output
        """
        cmd = ['svn', 'status', repo_dir_path]
        svn_output = execute_subprocess(cmd, output_to_caller=True)
        return svn_output

    @staticmethod
    def _svn_status_xml(repo_dir_path):
        """
        Get status of the subversion sandbox in repo_dir
        """
        cmd = ['svn', 'status', '--xml', repo_dir_path]
        svn_output = execute_subprocess(cmd, output_to_caller=True)
        return svn_output

    # ----------------------------------------------------------------
    #
    # system call to svn for sideffects modifying the working tree
    #
    # ----------------------------------------------------------------
    @staticmethod
    def _svn_checkout(url, repo_dir_path, verbosity):
        """
        Checkout a subversion repository (repo_url) to checkout_dir.
        """
        cmd = ['svn', 'checkout', '--quiet', url, repo_dir_path]
        if verbosity >= VERBOSITY_VERBOSE:
            printlog('    {0}'.format(' '.join(cmd)))
        execute_subprocess(cmd)

    @staticmethod
    def _svn_switch(url, ignore_ancestry, verbosity):
        """
        Switch branches for in an svn sandbox
        """
        cmd = ['svn', 'switch', '--quiet']
        if ignore_ancestry:
            cmd.append('--ignore-ancestry')
        cmd.append(url)
        if verbosity >= VERBOSITY_VERBOSE:
            printlog('    {0}'.format(' '.join(cmd)))
        execute_subprocess(cmd)
