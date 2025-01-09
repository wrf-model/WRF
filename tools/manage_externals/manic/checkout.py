#!/usr/bin/env python3

"""
Tool to assemble repositories represented in a model-description file.

If loaded as a module (e.g., in a component's buildcpp), it can be used
to check the validity of existing subdirectories and load missing sources.
"""
from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function

import argparse
import logging
import os
import os.path
import sys

from manic.externals_description import create_externals_description
from manic.externals_description import read_externals_description_file
from manic.externals_status import check_safe_to_update_repos
from manic.sourcetree import SourceTree
from manic.utils import printlog, fatal_error
from manic.global_constants import VERSION_SEPERATOR, LOG_FILE_NAME

if sys.hexversion < 0x02070000:
    print(70 * '*')
    print('ERROR: {0} requires python >= 2.7.x. '.format(sys.argv[0]))
    print('It appears that you are running python {0}'.format(
        VERSION_SEPERATOR.join(str(x) for x in sys.version_info[0:3])))
    print(70 * '*')
    sys.exit(1)


# ---------------------------------------------------------------------
#
# User input
#
# ---------------------------------------------------------------------
def commandline_arguments(args=None):
    """Process the command line arguments

    Params: args - optional args. Should only be used during systems
    testing.

    Returns: processed command line arguments
    """
    description = '''

%(prog)s manages checking out groups of externals from revision
control based on an externals description file. By default only the
required externals are checkout out.

Running %(prog)s without the '--status' option will always attempt to
synchronize the working copy to exactly match the externals description.
'''

    epilog = '''
```
NOTE: %(prog)s *MUST* be run from the root of the source tree it
is managing. For example, if you cloned a repository with:

    $ git clone git@github.com/{SOME_ORG}/some-project some-project-dev

Then the root of the source tree is /path/to/some-project-dev. If you
obtained a sub-project via a checkout of another project:

    $ git clone git@github.com/{SOME_ORG}/some-project some-project-dev

and you need to checkout the sub-project externals, then the root of the
source tree remains /path/to/some-project-dev. Do *NOT* run %(prog)s
from within /path/to/some-project-dev/sub-project

The root of the source tree will be referred to as `${SRC_ROOT}` below.


# Supported workflows

  * Checkout all required components from the default externals
    description file:

        $ cd ${SRC_ROOT}
        $ ./manage_externals/%(prog)s

  * To update all required components to the current values in the
    externals description file, re-run %(prog)s:

        $ cd ${SRC_ROOT}
        $ ./manage_externals/%(prog)s

    If there are *any* modifications to *any* working copy according
    to the git or svn 'status' command, %(prog)s
    will not update any external repositories. Modifications
    include: modified files, added files, removed files, or missing
    files.

    To avoid this safety check, edit the externals description file
    and comment out the modified external block.

  * Checkout all required components from a user specified externals
    description file:

        $ cd ${SRC_ROOT}
        $ ./manage_externals/%(prog)s --externals my-externals.cfg

  * Status summary of the repositories managed by %(prog)s:

        $ cd ${SRC_ROOT}
        $ ./manage_externals/%(prog)s --status

              ./cime
          s   ./components/cism
              ./components/mosart
          e-o ./components/rtm
           M  ./src/fates
          e-o ./tools/PTCLM


    where:
      * column one indicates the status of the repository in relation
        to the externals description file.
      * column two indicates whether the working copy has modified files.
      * column three shows how the repository is managed, optional or required

    Column one will be one of these values:
      * s : out-of-sync : repository is checked out at a different commit
            compared with the externals description
      * e : empty : directory does not exist - %(prog)s has not been run
      * ? : unknown : directory exists but .git or .svn directories are missing

    Column two will be one of these values:
      * M : Modified : modified, added, deleted or missing files
      *   : blank / space : clean
      * - : dash : no meaningful state, for empty repositories

    Column three will be one of these values:
      * o : optional : optionally repository
      *   : blank / space : required repository

  * Detailed git or svn status of the repositories managed by %(prog)s:

        $ cd ${SRC_ROOT}
        $ ./manage_externals/%(prog)s --status --verbose

# Externals description file

  The externals description contains a list of the external
  repositories that are used and their version control locations. The
  file format is the standard ini/cfg configuration file format. Each
  external is defined by a section containing the component name in
  square brackets:

  * name (string) : component name, e.g. [cime], [cism], etc.

  Each section has the following keyword-value pairs:

  * required (boolean) : whether the component is a required checkout,
    'true' or 'false'.

  * local_path (string) : component path *relative* to where
    %(prog)s is called.

  * protoctol (string) : version control protocol that is used to
    manage the component.  Valid values are 'git', 'svn',
    'externals_only'.

    Switching an external between different protocols is not
    supported, e.g. from svn to git. To switch protocols, you need to
    manually move the old working copy to a new location.

    Note: 'externals_only' will only process the external's own
    external description file without trying to manage a repository
    for the component. This is used for retrieving externals for
    standalone components like cam and ctsm which also serve as
    sub-components within a larger project. If the source root of the
    externals_only component is the same as the main source root, then
    the local path must be set to '.', the unix current working
    directory, e. g. 'local_path = .'

  * repo_url (string) : URL for the repository location, examples:
    * https://svn-ccsm-models.cgd.ucar.edu/glc
    * git@github.com:esmci/cime.git
    * /path/to/local/repository
    * .

    NOTE: To operate on only the local clone and and ignore remote
    repositories, set the url to '.' (the unix current path),
    i.e. 'repo_url = .' . This can be used to checkout a local branch
    instead of the upstream branch.

    If a repo url is determined to be a local path (not a network url)
    then user expansion, e.g. ~/, and environment variable expansion,
    e.g. $HOME or $REPO_ROOT, will be performed.

    Relative paths are difficult to get correct, especially for mixed
    use repos. It is advised that local paths expand to absolute paths.
    If relative paths are used, they should be relative to one level
    above local_path. If local path is 'src/foo', the the relative url
    should be relative to 'src'.

  * tag (string) : tag to checkout

  * hash (string) : the git hash to checkout. Only applies to git
    repositories.

  * branch (string) : branch to checkout from the specified
    repository. Specifying a branch on a remote repository means that
    %(prog)s will checkout the version of the branch in the remote,
    not the the version in the local repository (if it exists).

    Note: one and only one of tag, branch hash must be supplied.

  * externals (string) : used to make manage_externals aware of
    sub-externals required by an external. This is a relative path to
    the external's root directory. For example, if LIBX is often used
    as a sub-external, it might have an externals file (for its
    externals) called Externals_LIBX.cfg. To use libx as a standalone
    checkout, it would have another file, Externals.cfg with the
    following entry:

    [ libx ]
    local_path = .
    protocol = externals_only
    externals = Externals_LIBX.cfg
    required = True

    Now, %(prog)s will process Externals.cfg and also process
    Externals_LIBX.cfg as if it was a sub-external.

    Note that by default, checkout_externals will clone an external's
    submodules. As a special case, the entry, "externals = None", will
    prevent this behavior. For more control over which externals are
    checked out, create an externals file (and see the from_submodule
    configuration entry below).

  * from_submodule (True / False) : used to pull the repo_url, local_path,
    and hash properties for this external from the .gitmodules file in
    this repository. Note that the section name (the entry in square
    brackets) must match the name in the .gitmodules file.
    If from_submodule is True, the protocol must be git and no repo_url,
    local_path, hash, branch, or tag entries are allowed.
    Default: False

  * sparse (string) : used to control a sparse checkout. This optional
    entry should point to a filename (path relative to local_path) that
    contains instructions on which repository paths to include (or
    exclude) from the working tree.
    See the "SPARSE CHECKOUT" section of https://git-scm.com/docs/git-read-tree
    Default: sparse checkout is disabled

  * Lines beginning with '#' or ';' are comments and will be ignored.

# Obtaining this tool, reporting issues, etc.

  The master repository for manage_externals is
  https://github.com/ESMCI/manage_externals. Any issues with this tool
  should be reported there.

# Troubleshooting

Operations performed by manage_externals utilities are explicit and
data driven. %(prog)s will always attempt to make the working copy
*exactly* match what is in the externals file when modifying the
working copy of a repository.

If %(prog)s is not doing what you expected, double check the contents
of the externals description file or examine the output of
./manage_externals/%(prog)s --status

'''

    parser = argparse.ArgumentParser(
        description=description, epilog=epilog,
        formatter_class=argparse.RawDescriptionHelpFormatter)

    #
    # user options
    #
    parser.add_argument("components", nargs="*",
                        help="Specific component(s) to checkout. By default, "
                        "all required externals are checked out.")

    parser.add_argument('-e', '--externals', nargs='?',
                        default='Externals.cfg',
                        help='The externals description filename. '
                        'Default: %(default)s.')

    parser.add_argument('-x', '--exclude', nargs='*',
                        help='Component(s) listed in the externals file which should be ignored.')

    parser.add_argument('-o', '--optional', action='store_true', default=False,
                        help='By default only the required externals '
                        'are checked out. This flag will also checkout the '
                        'optional externals.')

    parser.add_argument('-S', '--status', action='store_true', default=False,
                        help='Output the status of the repositories managed by '
                        '%(prog)s. By default only summary information '
                        'is provided. Use the verbose option to see details.')

    parser.add_argument('-v', '--verbose', action='count', default=0,
                        help='Output additional information to '
                        'the screen and log file. This flag can be '
                        'used up to two times, increasing the '
                        'verbosity level each time.')

    parser.add_argument('--version', action='store_true', default=False,
                        help='Print manage_externals version and exit.')
    
    parser.add_argument('--svn-ignore-ancestry', action='store_true', default=False,
                        help='By default, subversion will abort if a component is '
                        'already checked out and there is no common ancestry with '
                        'the new URL. This flag passes the "--ignore-ancestry" flag '
                        'to the svn switch call. (This is not recommended unless '
                        'you are sure about what you are doing.)')

    #
    # developer options
    #
    parser.add_argument('--backtrace', action='store_true',
                        help='DEVELOPER: show exception backtraces as extra '
                        'debugging output')

    parser.add_argument('-d', '--debug', action='store_true', default=False,
                        help='DEVELOPER: output additional debugging '
                        'information to the screen and log file.')

    logging_group = parser.add_mutually_exclusive_group()

    logging_group.add_argument('--logging', dest='do_logging',
                               action='store_true',
                               help='DEVELOPER: enable logging.')
    logging_group.add_argument('--no-logging', dest='do_logging',
                               action='store_false', default=False,
                               help='DEVELOPER: disable logging '
                               '(this is the default)')

    if args:
        options = parser.parse_args(args)
    else:
        options = parser.parse_args()
    return options

def _dirty_local_repo_msg(program_name, config_file):
    return """The external repositories labeled with 'M' above are not in a clean state.
The following are four options for how to proceed:
(1) Go into each external that is not in a clean state and issue either a 'git status' or
    an 'svn status' command (depending on whether the external is managed by git or
    svn). Either revert or commit your changes so that all externals are in a clean
    state. (To revert changes in git, follow the instructions given when you run 'git
    status'.) (Note, though, that it is okay to have untracked files in your working
    directory.) Then rerun {program_name}.
(2) Alternatively, you do not have to rely on {program_name}. Instead, you can manually
    update out-of-sync externals (labeled with 's' above) as described in the
    configuration file {config_file}. (For example, run 'git fetch' and 'git checkout'
    commands to checkout the appropriate tags for each external, as given in
    {config_file}.)
(3) You can also use {program_name} to manage most, but not all externals: You can specify
    one or more externals to ignore using the '-x' or '--exclude' argument to
    {program_name}. Excluding externals labeled with 'M' will allow {program_name} to
    update the other, non-excluded externals.
(4) As a last resort, if you are confident that there is no work that needs to be saved
    from a given external, you can remove that external (via "rm -rf [directory]") and
    then rerun the {program_name} tool. This option is mainly useful as a workaround for
    issues with this tool (such as https://github.com/ESMCI/manage_externals/issues/157).
The external repositories labeled with '?' above are not under version
control using the expected protocol. If you are sure you want to switch
protocols, and you don't have any work you need to save from this
directory, then run "rm -rf [directory]" before rerunning the
{program_name} tool.
""".format(program_name=program_name, config_file=config_file)
# ---------------------------------------------------------------------
#
# main
#
# ---------------------------------------------------------------------
def main(args):
    """
    Function to call when module is called from the command line.
    Parse externals file and load required repositories or all repositories if
    the --all option is passed.

    Returns a tuple (overall_status, tree_status). overall_status is 0
    on success, non-zero on failure. tree_status is a dict mapping local path
    to ExternalStatus -- if no checkout is happening. If checkout is happening, tree_status
    is None.
    """
    if args.do_logging:
        logging.basicConfig(filename=LOG_FILE_NAME,
                            format='%(levelname)s : %(asctime)s : %(message)s',
                            datefmt='%Y-%m-%d %H:%M:%S',
                            level=logging.DEBUG)

    program_name = os.path.basename(sys.argv[0])
    logging.info('Beginning of %s', program_name)

    load_all = False
    if args.optional:
        load_all = True

    root_dir = os.path.abspath(os.getcwd())
    model_data = read_externals_description_file(root_dir, args.externals)
    ext_description = create_externals_description(
        model_data, components=args.components, exclude=args.exclude)

    for comp in args.components:
        if comp not in ext_description.keys():
            # Note we can't print out the list of found externals because
            # they were filtered in create_externals_description above.
            fatal_error(
                "No component {} found in {}".format(
                    comp, args.externals))

    source_tree = SourceTree(root_dir, ext_description, svn_ignore_ancestry=args.svn_ignore_ancestry)
    if args.components:
        components_str = 'specified components'
    else:
        components_str = 'required & optional components'
    printlog('Checking local status of ' + components_str + ': ', end='')
    tree_status = source_tree.status(print_progress=True)
    printlog('')

    if args.status:
        # user requested status-only
        for comp in sorted(tree_status):
            tree_status[comp].log_status_message(args.verbose)
    else:
        # checkout / update the external repositories.
        safe_to_update = check_safe_to_update_repos(tree_status)
        if not safe_to_update:
            # print status
            for comp in sorted(tree_status):
                tree_status[comp].log_status_message(args.verbose)
            # exit gracefully
            printlog('-' * 70)
            printlog(_dirty_local_repo_msg(program_name, args.externals))
            printlog('-' * 70)
        else:
            if not args.components:
                source_tree.checkout(args.verbose, load_all)
            for comp in args.components:
                source_tree.checkout(args.verbose, load_all, load_comp=comp)
            printlog('')
            # New tree status is unknown, don't return anything.
            tree_status = None

    logging.info('%s completed without exceptions.', program_name)
    # NOTE(bja, 2017-11) tree status is used by the systems tests
    return 0, tree_status
