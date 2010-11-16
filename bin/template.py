import logging
import optparse
import sys

try:
    from logging import NullHandler
except ImportError:
    class NullHandler(logging.Handler):
        def emit(self, record):
            pass

logging.getLogger("template").addHandler(NullHandler())

def parseargs(args):
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

    (opts, args) = parser.parse_args()
    return (opts, args)

def main():
    (opts, args) = parseargs(sys.argv[1:])
    level = logging.WARNING - ((opts.verbose - opts.quiet) * 10)
    if opts.silent:
        level = logging.CRITICAL + 1

    format = "%(message)s"
    logger = logging.getLogger("template")
    handler = logging.StreamHandler(sys.stderr)
    handler.setFormatter(logging.Formatter(format))
    logger.addHandler(handler)
    logger.setLevel(level)

    try:
        returned = do_stuff()
    except (KeyboardInterrupt,), e:
        returned = None

    sys.exit(returned and returned or 0)

if __name__ == "__main__":
    main()
