from mercurial import cmdutil, commands
from mercurial.extensions import wrapcommand
from mercurial.i18n import gettext, _
from hgext import bookmarks

# XXX: should we decorate "in", too?
delegated = ("outgoing", "pull", "push")

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
    elif target is None:
        target = "default"

    return kept(ui, repo, target, **opts)

def uisetup(ui):
    """Initialize the extension."""
    for delegatedcmd in delegated:
        wrapcommand(commands.table, delegatedcmd, bookdelegate)
