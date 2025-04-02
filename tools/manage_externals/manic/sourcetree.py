"""
Classes to represent an externals config file (SourceTree) and the components
within it (_External).
"""

import errno
import logging
import os

from .externals_description import ExternalsDescription
from .externals_description import read_externals_description_file
from .externals_description import create_externals_description
from .repository_factory import create_repository
from .repository_git import GitRepository
from .externals_status import ExternalStatus
from .utils import fatal_error, printlog
from .global_constants import EMPTY_STR, LOCAL_PATH_INDICATOR
from .global_constants import VERBOSITY_VERBOSE

class _External(object):
    """
    A single component hosted in an external repository (and any children).

    The component may or may not be checked-out upon construction.
    """
    # pylint: disable=R0902

    def __init__(self, root_dir, name, local_path, required, subexternals_path,
                 repo, svn_ignore_ancestry, subexternal_sourcetree):
        """Create a single external component (checked out or not).

        Input:
            root_dir : string - the (checked-out) parent repo's root dir.
            local_path : string - this external's (checked-out) subdir relative
            to root_dir, e.g. "components/mom"
            repo: Repository - the repo object for this external. Can be None (e.g. if this external just refers to another external file).

            name : string - name of this external (as named by the parent 
            reference).  May or may not correspond to something in the path.

            ext_description : dict - source ExternalsDescription object

            svn_ignore_ancestry : bool - use --ignore-externals with svn switch

            subexternals_path: string - path to sub-externals config file, if any. Relative to local_path, or special value 'none'.
            subexternal_sourcetree: SourceTree - corresponding to subexternals_path, if subexternals_path exists (it might not, if it is not checked out yet).
        """
        self._name = name
        self._required = required
        
        self._stat = None  # Populated in status()

        self._local_path = local_path
        # _repo_dir_path : full repository directory, e.g.
        # "<root_dir>/components/mom"
        repo_dir = os.path.join(root_dir, local_path)
        self._repo_dir_path = os.path.abspath(repo_dir)
        # _base_dir_path : base directory *containing* the repository, e.g.
        # "<root_dir>/components"
        self._base_dir_path = os.path.dirname(self._repo_dir_path)
        # _repo_dir_name : base_dir_path + repo_dir_name = repo_dir_path
        # e.g., "mom"
        self._repo_dir_name = os.path.basename(self._repo_dir_path)
        self._repo = repo

        # Does this component have subcomponents aka an externals config?
        self._subexternals_path = subexternals_path
        self._subexternal_sourcetree = subexternal_sourcetree
        

    def get_name(self):
        """
        Return the external object's name
        """
        return self._name

    def get_local_path(self):
        """
        Return the external object's path
        """
        return self._local_path

    def get_repo_dir_path(self):
        return self._repo_dir_path

    def get_subexternals_path(self):
        return self._subexternals_path

    def get_repo(self):
        return self._repo
    
    def status(self, force=False, print_progress=False):
        """
        Returns status of this component and all subcomponents.

        Returns a dict mapping our local path (not component name!) to an
        ExternalStatus dict. Any subcomponents will have their own top-level
        path keys.  Note the return value includes entries for this and all 
        subcomponents regardless of whether they are locally installed or not.

        Side-effect: If self._stat is empty or force is True, calculates _stat.
        """
        calc_stat = force or not self._stat

        if calc_stat:
            self._stat = ExternalStatus()
            self._stat.path = self.get_local_path()
            if not self._required:
                self._stat.source_type = ExternalStatus.OPTIONAL
            elif self._local_path == LOCAL_PATH_INDICATOR:
                # LOCAL_PATH_INDICATOR, '.' paths, are standalone
                # component directories that are not managed by
                # checkout_subexternals.
                self._stat.source_type = ExternalStatus.STANDALONE
            else:
                # managed by checkout_subexternals
                self._stat.source_type = ExternalStatus.MANAGED

        subcomponent_stats = {}
        if not os.path.exists(self._repo_dir_path):
            if calc_stat:
                # No local repository.
                self._stat.sync_state = ExternalStatus.EMPTY
                msg = ('status check: repository directory for "{0}" does not '
                       'exist.'.format(self._name))
                logging.info(msg)
                self._stat.current_version = 'not checked out'
                # NOTE(bja, 2018-01) directory doesn't exist, so we cannot
                # use repo to determine the expected version. We just take
                # a best-guess based on the assumption that only tag or
                # branch should be set, but not both.
                if not self._repo:
                    self._stat.expected_version = 'unknown'
                else:
                    self._stat.expected_version = self._repo.tag() + self._repo.branch()
        else:
            # Merge local repository state (e.g. clean/dirty) into self._stat.
            if calc_stat and self._repo:
                self._repo.status(self._stat, self._repo_dir_path)

            # Status of subcomponents, if any.
            if self._subexternals_path and self._subexternal_sourcetree:
                cwd = os.getcwd()
                # SourceTree.status() expects to be called from the correct
                # root directory.
                os.chdir(self._repo_dir_path)
                subcomponent_stats = self._subexternal_sourcetree.status(self._local_path, force=force, print_progress=print_progress)
                os.chdir(cwd)

        # Merge our status + subcomponent statuses into one return dict keyed
        # by component path.
        all_stats = {}
        # don't add the root component because we don't manage it
        # and can't provide useful info about it.
        if self._local_path != LOCAL_PATH_INDICATOR:
            # store the stats under the local_path, not comp name so
            # it will be sorted correctly
            all_stats[self._stat.path] = self._stat

        if subcomponent_stats:
            all_stats.update(subcomponent_stats)

        return all_stats

    def checkout(self, verbosity):
        """
        If the repo destination directory exists, ensure it is correct (from
        correct URL, correct branch or tag), and possibly updateit.
        If the repo destination directory does not exist, checkout the correct
        branch or tag.
        Does not check out sub-externals, see SourceTree.checkout().
        """
        # Make sure we are in correct location
        if not os.path.exists(self._repo_dir_path):
            # repository directory doesn't exist. Need to check it
            # out, and for that we need the base_dir_path to exist
            try:
                os.makedirs(self._base_dir_path)
            except OSError as error:
                if error.errno != errno.EEXIST:
                    msg = 'Could not create directory "{0}"'.format(
                        self._base_dir_path)
                    fatal_error(msg)

        if not self._stat:
            self.status()
            assert self._stat
            
        if self._stat.source_type != ExternalStatus.STANDALONE:
            if verbosity >= VERBOSITY_VERBOSE:
                # NOTE(bja, 2018-01) probably do not want to pass
                # verbosity in this case, because if (verbosity ==
                # VERBOSITY_DUMP), then the previous status output would
                # also be dumped, adding noise to the output.
                self._stat.log_status_message(VERBOSITY_VERBOSE)

        if self._repo:
            if self._stat.sync_state == ExternalStatus.STATUS_OK:
                # If we're already in sync, avoid showing verbose output
                # from the checkout command, unless the verbosity level
                # is 2 or more.
                checkout_verbosity = verbosity - 1
            else:
                checkout_verbosity = verbosity

            self._repo.checkout(self._base_dir_path, self._repo_dir_name,
                                checkout_verbosity, self.clone_recursive())

    def replace_subexternal_sourcetree(self, sourcetree):
        self._subexternal_sourcetree = sourcetree

    def clone_recursive(self):
        'Return True iff any .gitmodules files should be processed'
        # Try recursive .gitmodules unless there is an externals entry
        recursive = not self._subexternals_path

        return recursive


class SourceTree(object):
    """
    SourceTree represents a group of managed externals.

    Those externals may not be checked out locally yet, they might only
    have Repository objects pointing to their respective repositories.
    """

    @classmethod
    def from_externals_file(cls, parent_repo_dir_path, parent_repo,
                            externals_path):
        """Creates a SourceTree representing the given externals file.
        
        Looks up a git submodules file as an optional backup if there is no
        externals file specified.

        Returns None if there is no externals file (i.e. it's None or 'none'),
        or if the externals file hasn't been checked out yet.

        parent_repo_dir_path: parent repo root dir
        parent_repo: parent repo.
        externals_path: path to externals file, relative to parent_repo_dir_path.
        """
        if not os.path.exists(parent_repo_dir_path):
            # NOTE(bja, 2017-10) repository has not been checked out
            # yet, can't process the externals file. Assume we are
            # checking status before code is checkoud out and this
            # will be handled correctly later.
            return None

        if externals_path.lower() == 'none':
            # With explicit 'none', do not look for git submodules file.
            return None

        cwd = os.getcwd()
        os.chdir(parent_repo_dir_path)
        
        if not externals_path:
            if GitRepository.has_submodules(parent_repo_dir_path):
                externals_path = ExternalsDescription.GIT_SUBMODULES_FILENAME
            else:
                return None

        if not os.path.exists(externals_path):
            # NOTE(bja, 2017-10) this check is redundant with the one
            # in read_externals_description_file!
            msg = ('Externals description file "{0}" '
                   'does not exist! In directory: {1}'.format(
                       externals_path, parent_repo_dir_path))
            fatal_error(msg)

        externals_root = parent_repo_dir_path
        # model_data is a dict-like object which mirrors the file format.
        model_data = read_externals_description_file(externals_root,
                                                     externals_path)
        # ext_description is another dict-like object (see ExternalsDescription)
        ext_description = create_externals_description(model_data,
                                                       parent_repo=parent_repo)
        externals_sourcetree = SourceTree(externals_root, ext_description)
        os.chdir(cwd)
        return externals_sourcetree
    
    def __init__(self, root_dir, ext_description, svn_ignore_ancestry=False):
        """
        Build a SourceTree object from an ExternalDescription.

        root_dir: the (checked-out) parent repo root dir.
        """
        self._root_dir = os.path.abspath(root_dir)
        self._all_components = {}  # component_name -> _External
        self._required_compnames = []
        for comp, desc in ext_description.items():
            local_path = desc[ExternalsDescription.PATH]
            required = desc[ExternalsDescription.REQUIRED]
            repo_info = desc[ExternalsDescription.REPO]
            subexternals_path = desc[ExternalsDescription.EXTERNALS]

            repo = create_repository(comp,
                                     repo_info,
                                     svn_ignore_ancestry=svn_ignore_ancestry)

            sourcetree = None
            # Treat a .gitmodules file as a backup externals config
            if not subexternals_path:
                parent_repo_dir_path = os.path.abspath(os.path.join(root_dir,
                                                             local_path))
                if GitRepository.has_submodules(parent_repo_dir_path):
                    subexternals_path = ExternalsDescription.GIT_SUBMODULES_FILENAME
            
            # Might return None (if the subexternal isn't checked out yet, or subexternal is None or 'none')
            subexternal_sourcetree = SourceTree.from_externals_file(
                os.path.join(self._root_dir, local_path),
                repo,
                subexternals_path)
            src = _External(self._root_dir, comp, local_path, required,
                            subexternals_path, repo, svn_ignore_ancestry,
                            subexternal_sourcetree)
            
            self._all_components[comp] = src
            if required:
                self._required_compnames.append(comp)

    def status(self, relative_path_base=LOCAL_PATH_INDICATOR,
               force=False, print_progress=False):
        """Return a dictionary of local path->ExternalStatus.

        Notes about the returned dictionary:
          * It is keyed by local path (e.g. 'components/mom'), not by
            component name (e.g. 'mom').
          * It contains top-level keys for all traversed components, whether
            discovered by recursion or top-level.
          * It contains entries for all components regardless of whether they
            are locally installed or not, or required or optional.
x        """
        load_comps = self._all_components.keys()

        summary = {}  # Holds merged statuses from all components.
        for comp in load_comps:
            if print_progress:
                printlog('{0}, '.format(comp), end='')
            stat = self._all_components[comp].status(force=force,
                                                     print_progress=print_progress)

            # Returned status dictionary is keyed by local path; prepend
            # relative_path_base if not already there.
            stat_final = {}
            for name in stat.keys():
                if stat[name].path.startswith(relative_path_base):
                    stat_final[name] = stat[name]
                else:
                    modified_path = os.path.join(relative_path_base,
                                                 stat[name].path)
                    stat_final[modified_path] = stat[name]
                    stat_final[modified_path].path = modified_path
            summary.update(stat_final)

        return summary

    def _find_installed_optional_components(self):
        """Returns a list of installed optional component names, if any."""
        installed_comps = []
        for comp_name, ext in self._all_components.items():
            if comp_name in self._required_compnames:
                continue
            # Note that in practice we expect this status to be cached.
            path_to_stat = ext.status()

            # If any part of this component exists locally, consider it
            # installed and therefore eligible for updating.
            if any(s.sync_state != ExternalStatus.EMPTY
                   for s in path_to_stat.values()):
                installed_comps.append(comp_name)
        return installed_comps

    def checkout(self, verbosity, load_all, load_comp=None):
        """
        Checkout or update indicated components into the configured subdirs.

        If load_all is True, checkout all externals (required + optional), recursively.
        If load_all is False and load_comp is set, checkout load_comp (and any required subexternals, plus any optional subexternals that are already checked out, recursively)
        If load_all is False and load_comp is None, checkout all required externals, plus any optionals that are already checked out, recursively.
        """
        if load_all:
            tmp_comps = self._all_components.keys()
        elif load_comp is not None:
            tmp_comps = [load_comp]
        else:
            local_optional_compnames = self._find_installed_optional_components()
            tmp_comps = self._required_compnames + local_optional_compnames
            if local_optional_compnames:
                printlog('Found locally installed optional components: ' +
                         ', '.join(local_optional_compnames))
                bad_compnames = set(local_optional_compnames) - set(self._all_components.keys())
                if bad_compnames:
                    printlog('Internal error: found locally installed components that are not in the global list of all components: ' + ','.join(bad_compnames))

        if verbosity >= VERBOSITY_VERBOSE:
            printlog('Checking out externals: ')
        else:
            printlog('Checking out externals: ', end='')

        # Sort by path so that if paths are nested the
        # parent repo is checked out first.
        load_comps = sorted(tmp_comps, key=lambda comp: self._all_components[comp].get_local_path())

        # checkout.
        for comp_name in load_comps:
            if verbosity < VERBOSITY_VERBOSE:
                printlog('{0}, '.format(comp_name), end='')
            else:
                # verbose output handled by the _External object, just
                # output a newline
                printlog(EMPTY_STR)
            c = self._all_components[comp_name]
            # Does not recurse.
            c.checkout(verbosity)
            # Recursively check out subexternals, if any. Returns None
            # if there's no subexternals path.
            component_subexternal_sourcetree = SourceTree.from_externals_file(
                    c.get_repo_dir_path(),
                    c.get_repo(),
                    c.get_subexternals_path())
            c.replace_subexternal_sourcetree(component_subexternal_sourcetree)
            if component_subexternal_sourcetree:
                component_subexternal_sourcetree.checkout(verbosity, load_all)
        printlog('')
