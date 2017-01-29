import os


def acme():
    import sys
    import traceback
    write = sys.stderr.write

    def acmehook(typ, value, tb):
        for name, line, fn, text in traceback.extract_tb(tb):
            write("{}:{} in {}, {}\n".format(name, line, fn, text))
        write("{}.{}: {}\n".format(typ.__module__, typ.__name__, value))

    sys.excepthook = acmehook


if os.getenv("winid"):
    acme()
del (os, acme)
