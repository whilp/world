"wrapper"

def wrap(name, file, args = None, data = None):
    if args == None:
        args = []
    if data == None:
        data = []

    native.sh_binary(
        name = name,
        srcs = ["//cmd:wrapper.sh"],
        args = ["$(location %s)" % file] + args,
        data = data + [file],
    )
