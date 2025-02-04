#!/usr/bin/env python3
"""
Common public utilities for manic package

"""

from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function

import logging
import os
import subprocess
import sys
from threading import Timer

from .global_constants import LOCAL_PATH_INDICATOR

# ---------------------------------------------------------------------
#
# screen and logging output and functions to massage text for output
#
# ---------------------------------------------------------------------


def log_process_output(output):
    """Log each line of process output at debug level so it can be
    filtered if necessary. By default, output is a single string, and
    logging.debug(output) will only put log info heading on the first
    line. This makes it hard to filter with grep.

    """
    output = output.split('\n')
    for line in output:
        logging.debug(line)


def printlog(msg, **kwargs):
    """Wrapper script around print to ensure that everything printed to
    the screen also gets logged.

    """
    logging.info(msg)
    if kwargs:
        print(msg, **kwargs)
    else:
        print(msg)
    sys.stdout.flush()


def last_n_lines(the_string, n_lines, truncation_message=None):
    """Returns the last n lines of the given string

    Args:
        the_string: str
        n_lines: int
        truncation_message: str, optional

    Returns a string containing the last n lines of the_string

    If truncation_message is provided, the returned string begins with
    the given message if and only if the string is greater than n lines
    to begin with.
    """

    lines = the_string.splitlines(True)
    if len(lines) <= n_lines:
        return_val = the_string
    else:
        lines_subset = lines[-n_lines:]
        str_truncated = ''.join(lines_subset)
        if truncation_message:
            str_truncated = truncation_message + '\n' + str_truncated
        return_val = str_truncated

    return return_val


def indent_string(the_string, indent_level):
    """Indents the given string by a given number of spaces

    Args:
       the_string: str
       indent_level: int

    Returns a new string that is the same as the_string, except that
    each line is indented by 'indent_level' spaces.

    In python3, this can be done with textwrap.indent.
    """

    lines = the_string.splitlines(True)
    padding = ' ' * indent_level
    lines_indented = [padding + line for line in lines]
    return ''.join(lines_indented)

# ---------------------------------------------------------------------
#
# error handling
#
# ---------------------------------------------------------------------


def fatal_error(message):
    """
    Error output function
    """
    logging.error(message)
    raise RuntimeError("{0}ERROR: {1}".format(os.linesep, message))


# ---------------------------------------------------------------------
#
# Data conversion / manipulation
#
# ---------------------------------------------------------------------
def str_to_bool(bool_str):
    """Convert a sting representation of as boolean into a true boolean.

    Conversion should be case insensitive.
    """
    value = None
    str_lower = bool_str.lower()
    if str_lower in ('true', 't'):
        value = True
    elif str_lower in ('false', 'f'):
        value = False
    if value is None:
        msg = ('ERROR: invalid boolean string value "{0}". '
               'Must be "true" or "false"'.format(bool_str))
        fatal_error(msg)
    return value


REMOTE_PREFIXES = ['http://', 'https://', 'ssh://', 'git@']


def is_remote_url(url):
    """check if the user provided a local file path instead of a
       remote. If so, it must be expanded to an absolute
       path.

    """
    remote_url = False
    for prefix in REMOTE_PREFIXES:
        if url.startswith(prefix):
            remote_url = True
    return remote_url


def split_remote_url(url):
    """check if the user provided a local file path or a
       remote. If remote, try to strip off protocol info.

    """
    remote_url = is_remote_url(url)
    if not remote_url:
        return url

    for prefix in REMOTE_PREFIXES:
        url = url.replace(prefix, '')

    if '@' in url:
        url = url.split('@')[1]

    if ':' in url:
        url = url.split(':')[1]

    return url


def expand_local_url(url, field):
    """check if the user provided a local file path instead of a
    remote. If so, it must be expanded to an absolute
    path.

    Note: local paths of LOCAL_PATH_INDICATOR have special meaning and
    represent local copy only, don't work with the remotes.

    """
    remote_url = is_remote_url(url)
    if not remote_url:
        if url.strip() == LOCAL_PATH_INDICATOR:
            pass
        else:
            url = os.path.expandvars(url)
            url = os.path.expanduser(url)
            if not os.path.isabs(url):
                msg = ('WARNING: Externals description for "{0}" contains a '
                       'url that is not remote and does not expand to an '
                       'absolute path. Version control operations may '
                       'fail.\n\nurl={1}'.format(field, url))
                printlog(msg)
            else:
                url = os.path.normpath(url)
    return url


# ---------------------------------------------------------------------
#
# subprocess
#
# ---------------------------------------------------------------------

# Give the user a helpful message if we detect that a command seems to
# be hanging.
_HANGING_SEC = 300


def _hanging_msg(working_directory, command):
    print("""

Command '{command}'
from directory {working_directory}
has taken {hanging_sec} seconds. It may be hanging.

The command will continue to run, but you may want to abort
manage_externals with ^C and investigate. A possible cause of hangs is
when svn or git require authentication to access a private
repository. On some systems, svn and git requests for authentication
information will not be displayed to the user. In this case, the program
will appear to hang. Ensure you can run svn and git manually and access
all repositories without entering your authentication information.

""".format(command=command,
           working_directory=working_directory,
           hanging_sec=_HANGING_SEC))


def execute_subprocess(commands, status_to_caller=False,
                       output_to_caller=False):
    """Wrapper around subprocess.check_output to handle common
    exceptions.

    check_output runs a command with arguments and waits
    for it to complete.

    check_output raises an exception on a nonzero return code.  if
    status_to_caller is true, execute_subprocess returns the subprocess
    return code, otherwise execute_subprocess treats non-zero return
    status as an error and raises an exception.

    """
    cwd = os.getcwd()
    msg = 'In directory: {0}\nexecute_subprocess running command:'.format(cwd)
    logging.info(msg)
    commands_str = ' '.join(commands)
    logging.info(commands_str)
    return_to_caller = status_to_caller or output_to_caller
    status = -1
    output = ''
    hanging_timer = Timer(_HANGING_SEC, _hanging_msg,
                          kwargs={"working_directory": cwd,
                                  "command": commands_str})
    hanging_timer.start()
    try:
        output = subprocess.check_output(commands, stderr=subprocess.STDOUT,
                                         universal_newlines=True)
        log_process_output(output)
        status = 0
    except OSError as error:
        msg = failed_command_msg(
            'Command execution failed. Does the executable exist?',
            commands)
        logging.error(error)
        fatal_error(msg)
    except ValueError as error:
        msg = failed_command_msg(
            'DEV_ERROR: Invalid arguments trying to run subprocess',
            commands)
        logging.error(error)
        fatal_error(msg)
    except subprocess.CalledProcessError as error:
        # Only report the error if we are NOT returning to the
        # caller. If we are returning to the caller, then it may be a
        # simple status check. If returning, it is the callers
        # responsibility determine if an error occurred and handle it
        # appropriately.
        if not return_to_caller:
            msg_context = ('Process did not run successfully; '
                           'returned status {0}'.format(error.returncode))
            msg = failed_command_msg(msg_context, commands,
                                     output=error.output)
            logging.error(error)
            logging.error(msg)
            log_process_output(error.output)
            fatal_error(msg)
        status = error.returncode
    finally:
        hanging_timer.cancel()

    if status_to_caller and output_to_caller:
        ret_value = (status, output)
    elif status_to_caller:
        ret_value = status
    elif output_to_caller:
        ret_value = output
    else:
        ret_value = None

    return ret_value


def failed_command_msg(msg_context, command, output=None):
    """Template for consistent error messages from subprocess calls.

    If 'output' is given, it should provide the output from the failed
    command
    """

    if output:
        output_truncated = last_n_lines(output, 20,
                                        truncation_message='[... Output truncated for brevity ...]')
        errmsg = ('Failed with output:\n' +
                  indent_string(output_truncated, 4) +
                  '\nERROR: ')
    else:
        errmsg = ''

    command_str = ' '.join(command)
    errmsg += """In directory
    {cwd}
{context}:
    {command}
""".format(cwd=os.getcwd(), context=msg_context, command=command_str)

    if output:
        errmsg += 'See above for output from failed command.\n'

    return errmsg
