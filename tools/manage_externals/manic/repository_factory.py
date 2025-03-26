"""Factory for creating and initializing the appropriate repository class
"""

from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function

from .repository_git import GitRepository
from .repository_svn import SvnRepository
from .externals_description import ExternalsDescription
from .utils import fatal_error


def create_repository(component_name, repo_info, svn_ignore_ancestry=False):
    """Determine what type of repository we have, i.e. git or svn, and
    create the appropriate object.

    Can return None (e.g. if protocol is 'externals_only').
    """
    protocol = repo_info[ExternalsDescription.PROTOCOL].lower()
    if protocol == 'git':
        repo = GitRepository(component_name, repo_info)
    elif protocol == 'svn':
        repo = SvnRepository(component_name, repo_info, ignore_ancestry=svn_ignore_ancestry)
    elif protocol == 'externals_only':
        repo = None
    else:
        msg = 'Unknown repo protocol "{0}"'.format(protocol)
        fatal_error(msg)
    return repo
