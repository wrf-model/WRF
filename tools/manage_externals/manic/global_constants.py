"""Globals shared across modules
"""

from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function

import pprint

EMPTY_STR = ''
LOCAL_PATH_INDICATOR = '.'
VERSION_SEPERATOR = '.'
LOG_FILE_NAME = 'manage_externals.log'
PPRINTER = pprint.PrettyPrinter(indent=4)

VERBOSITY_DEFAULT = 0
VERBOSITY_VERBOSE = 1
VERBOSITY_DUMP = 2
