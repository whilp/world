import logging
import optparse
import os
import sys
import time

from optparse import Option

try:
    from logging import NullHandler
except ImportError:
    class NullHandler(logging.Handler):
        
        def emit(self, record):
            pass

logname = __name__
if __name__ == "__main__":
	logname = os.path.splitext(os.path.basename(sys.argv[0]))
logging.getLogger(logname).addHandler(NullHandler())

def getlogger(name, level, stream,
        fmt="%(asctime)s %(name)s %(message)s",
        datefmt="%Y-%m-%dT%H:%M:%S%z"):
    logger = logging.getLogger(name)
    logger.setLevel(level)

    handler = logging.StreamHandler(stream)
    formatter = Formatter(fmt, datefmt)
    handler.setFormatter(formatter)
    logger.addHandler(handler)

    return logger

def getcmd(name, commands):
    """Resolve *name* to a command registered in *commands*.

    *name* should be a string; *commands* is a dictionary mapping full command
    names to information about each command (help text, usage, etc). If *name*
    isn't found in *commands* but one (and only one) entry in the dictionary
    starts with *name*, use that entry as the command. If more or less than one
    entry matches, raise ValueError.

    Returns a tuple (name, command), where *name* and *command* are the matching
    key and value from *commands*.
    """
    command = commands.get(name, None)
    if command is not None:
        return (name, command)

    matches = [c for c in commands if c.startswith(name)]
    (name,) = matches
    return (name, commands[name])

def parseargs(args, commands={}):
    parser = optparse.OptionParser()
    parser.allow_interspersed_args = False

    # Global options.
    parser.add_option("-q", "--quiet", dest="quiet",
        default=0, action="count",
        help="decrease the logging verbosity")
    parser.add_option("-s", "--silent", dest="silent",
        default=False, action="store_true",
        help="silence the logger")
    parser.add_option("-v", "--verbose", dest="verbose",
        default=0, action="count",
        help="increase the logging verbosity")

    if commands:
        usage = ["%prog COMMAND\n\nCommands:"]
        for name, cmd  in commands.items():
            usage.append("  %14s  %s" % (name, cmd.help))
        parser.usage = '\n'.join(usage)

    (opts, args) = parser.parse_args()
    if commands and not args:
        parser.print_help()
        sys.exit(0)
    elif not commands:
        return (None, opts, None, args)

    cmdname = args[0]
    try:
        cmdname, command = getcmd(args.pop(0), commands)
    except ValueError, e:
        parser.error("invalid command %r" % cmdname)

    subparser = optparse.OptionParser()
    for opt in command.opts:
        subparser.add_option(opt)

    subparser.usage = "%%prog %s %s" % (cmdname, cmd.usage)

    (cmdopts, args) = subparser.parse_args(args)

    return (command, opts, cmdopts, args)

class Command(object):
    help = ""
    usage = ""
    opts = []

    def dispatch(self, globalopts, opts, args):
        """Run the command.

        *globalopts* and *opts* are :class:`optparse.Values` instances; *args*
        is a list of arguments. Returns None or an integer indicating the status
        of the command.
        """
        pass

commands = {
}

def main():
    (command, opts, cmdopts, args) = parseargs(sys.argv[1:], commands)
    level = logging.WARNING - ((opts.verbose - opts.quiet) * 10)
    if opts.silent:
        level = logging.CRITICAL + 1

    logger = getlogger(logname, level, sys.stderr)

    try:
        returned = command.dispatch(opts, cmdopts, args)
    except TypeError:
        sys.stderr.write("%s: invalid arguments\n" % logname)
        returned = 2
    except (KeyboardInterrupt,), e:
        returned = None

    sys.exit(returned and returned or 0)

if __name__ == "__main__":
    main()
