-- AUTOMATICALLY GENERATED FILE. DO NOT EDIT --

[![Build Status](https://travis-ci.org/ESMCI/manage_externals.svg?branch=master)](https://travis-ci.org/ESMCI/manage_externals)[![Coverage Status](https://coveralls.io/repos/github/ESMCI/manage_externals/badge.svg?branch=master)](https://coveralls.io/github/ESMCI/manage_externals?branch=master)
```
usage: checkout_externals [-h] [-e [EXTERNALS]] [-o] [-S] [-v] [--backtrace]
                          [-d] [--no-logging]

checkout_externals manages checking out groups of externals from revision
control based on a externals description file. By default only the
required externals are checkout out.

Operations performed by manage_externals utilities are explicit and
data driven. checkout_externals will always make the working copy *exactly*
match what is in the externals file when modifying the working copy of
a repository.

If checkout_externals isn't doing what you expected, double check the contents
of the externals description file.

Running checkout_externals without the '--status' option will always attempt to
synchronize the working copy to exactly match the externals description.

optional arguments:
  -h, --help            show this help message and exit
  -e [EXTERNALS], --externals [EXTERNALS]
                        The externals description filename. Default:
                        Externals.cfg.
  -o, --optional        By default only the required externals are checked
                        out. This flag will also checkout the optional
                        externals.
  -S, --status          Output status of the repositories managed by
                        checkout_externals. By default only summary
                        information is provided. Use verbose output to see
                        details.
  -v, --verbose         Output additional information to the screen and log
                        file. This flag can be used up to two times,
                        increasing the verbosity level each time.
  --backtrace           DEVELOPER: show exception backtraces as extra
                        debugging output
  -d, --debug           DEVELOPER: output additional debugging information to
                        the screen and log file.
  --no-logging          DEVELOPER: disable logging.

```
NOTE: checkout_externals *MUST* be run from the root of the source tree it
is managing. For example, if you cloned a repository with:

    $ git clone git@github.com/{SOME_ORG}/some-project some-project-dev

Then the root of the source tree is /path/to/some-project-dev. If you
obtained a sub-project via a checkout of another project:

    $ git clone git@github.com/{SOME_ORG}/some-project some-project-dev

and you need to checkout the sub-project externals, then the root of the
source tree is /path/to/some-project-dev. Do *NOT* run checkout_externals
from within /path/to/some-project-dev/sub-project

The root of the source tree will be referred to as `${SRC_ROOT}` below.

# Supported workflows

  * Checkout all required components from the default externals
    description file:

        $ cd ${SRC_ROOT}
        $ ./manage_externals/checkout_externals

  * To update all required components to the current values in the
    externals description file, re-run checkout_externals:

        $ cd ${SRC_ROOT}
        $ ./manage_externals/checkout_externals

    If there are *any* modifications to *any* working copy according
    to the git or svn 'status' command, checkout_externals
    will not update any external repositories. Modifications
    include: modified files, added files, removed files, or missing
    files.

    To avoid this safety check, edit the externals description file
    and comment out the modified external block.

  * Checkout all required components from a user specified externals
    description file:

        $ cd ${SRC_ROOT}
        $ ./manage_externals/checkout_externals --externals my-externals.cfg

  * Status summary of the repositories managed by checkout_externals:

        $ cd ${SRC_ROOT}
        $ ./manage_externals/checkout_externals --status

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
      * e : empty : directory does not exist - checkout_externals has not been run
      * ? : unknown : directory exists but .git or .svn directories are missing

    Column two will be one of these values:
      * M : Modified : modified, added, deleted or missing files
      *   : blank / space : clean
      * - : dash : no meaningful state, for empty repositories

    Column three will be one of these values:
      * o : optional : optionally repository
      *   : blank / space : required repository

  * Detailed git or svn status of the repositories managed by checkout_externals:

        $ cd ${SRC_ROOT}
        $ ./manage_externals/checkout_externals --status --verbose

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
    checkout_externals is called.

  * protoctol (string) : version control protocol that is used to
    manage the component.  Valid values are 'git', 'svn',
    'externals_only'.

    Switching an external between different protocols is not
    supported, e.g. from svn to git. To switch protocols, you need to
    manually move the old working copy to a new location.

    Note: 'externals_only' will only process the external's own
    external description file without trying to manage a repository
    for the component. This is used for retreiving externals for
    standalone components like cam and clm. If the source root of the
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
    checkout_externals will checkout the version of the branch in the remote,
    not the the version in the local repository (if it exists).

    Note: one and only one of tag, branch hash must be supplied.

  * externals (string) : used to make manage_externals aware of
    sub-externals required by an external. This is a relative path to
    the external's root directory. For example, the main externals
    description has an external checkout out at 'src/useful_library'.
    useful_library requires additional externals to be complete.
    Those additional externals are managed from the source root by the
    externals description file pointed 'useful_library/sub-xternals.cfg',
    Then the main 'externals' field in the top level repo should point to
    'sub-externals.cfg'.
    Note that by default, `checkout_externals` will clone an external's
    submodules. As a special case, the entry, `externals = None`, will
    prevent this behavior. For more control over which externals are
    checked out, create an externals file (and see the `from_submodule`
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

  * Lines begining with '#' or ';' are comments and will be ignored.

# Obtaining this tool, reporting issues, etc.

  The master repository for manage_externals is
  https://github.com/ESMCI/manage_externals. Any issues with this tool
  should be reported there.
