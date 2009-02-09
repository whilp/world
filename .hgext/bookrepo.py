from mercurial import cmdutil, commands
from mercurial.i18n import gettext, _
from hgext import bookmarks

# XXX: should we decorate "in", too?
delegated = ("outgoing", "pull", "push")

def decoratecmd(ui, cmd, table, delegate, *delegateoptions):
    """Decorate a command.

    The decorator that becomes the new implementation of cmd calls
    delegate.  The delegate's first argument is the replaced function,
    followed by the normal Mercurial command arguments (ui, repo, ...).  If
    the delegate adds command options, supply them as delegateoptions.

    Stolen (mostly) from hgext.color._decoratecmd and _cmdtableitem.
    """
    _, entry = cmdutil.findcmd(cmd, table)
    candidates = [(k, e) for k, e in table.items() if e is entry]
    if candidates:
        # XXX: what to do if there aren't any candidates?
        cmdkey, cmdentry = candidates[0]

    decorator = lambda ui, repo, *args, **opts: \
                    delegate(cmdentry[0], ui, repo, *args, **opts)

    # make sure 'hg help cmd' still works
    decorator.__doc__ = cmdentry[0].__doc__
    decoratorentry = (decorator,) + cmdentry[1:]
    for option in delegateoptions:
        decoratorentry[1].append(option)
    table[cmdkey] = decoratorentry

def bookdelegate(kept, ui, repo, target=None, **opts):
    bookmark = bookmarks.current(repo)
    paths = [name for name, path in ui.configitems("paths")]
    paths.insert(0, "main")

    if bookmark in paths:
        rev = bookmark

        if bookmark == "main":
            target = "default"
        elif target is None:
            target = bookmark
        elif target != bookmark:
            rev = bookmark

        if not opts["rev"]:
            opts["rev"] = [rev]

    return kept(ui, repo, target, **opts)

def uisetup(ui):
    """Initialize the extension."""
    for delegatedcmd in delegated:
        #commands.table.pop(delegatedcmd)
        decoratecmd(ui, delegatedcmd, commands.table, bookdelegate)
