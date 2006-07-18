#!/bin/sh
# Set a PKG_PATH, unless there is one already.

if [ ! $1 ]; then
	echo "'$0 pre' to prepare and run the upgrade or"
	echo "'$0 post' to clean up and merge"
	exit 1
fi

MY_PP=ftp://ftp5.usa.openbsd.org/pub/OpenBSD/snapshots/packages/i386/:ftp://mirrors.tds.net/pub/OpenBSD/snapshots/packages/i386/
PKG_PATH=${PKG_PATH:-${MY_PP}}
VERSION=39
WORKDIR="${HOME}/BSD"
BACKUPDIR=/home/backup

case "x$1" in
   xpre)

	# Fetch installsets.
	mkdir "${WORKDIR}"
	cd "${WORKDIR}"
	ftp ${PKG_PATH%%packages*}/$(machine)/\*

	# Backup /bsd and /bsd.rd; prepare to upgrade.
	sudo cp /bsd /bsd.old
	sudo cp /bsd.rd /bsd.rd.old
	sudo cp bsd.rd /bsd.rd
	sudo ln /bsd.rd /bsd        # In case the thing doesn't want to boot from bsd.rd.

	# Reboot into the upgrade process. Follow the instructions (like an
	# install).
	echo sudo shutdown -r now
	exit
	;;
   xpost)
	# Boot into the new system and update packages.
	sudo pkg_add -iu

	# Backup and merge changes to /etc, /var and /root (which aren't
	# covered by the upgrade process). I think /dev is covered by the
	# install process.
	cd "${WORKDIR}"
	mkdir merge
	tar xvzf "etc${VERSION}.tgz" -C merge/

	sudo mkdir "${BACKUPDIR}"
	DIRS="etc var root"
	for DIR in ${DIRS}; do
	    sudo tar cvzf "${BACKUPDIR}/${DIR}-$(date "+%Y%m%d-%H%M%S").tgz" "/${DIR}"
	done

	# Merge changes.
	sudo pkg_add mergemaster
	sudo mergemaster -rt "${WORKDIR}/merge"

	# Rebuild the password databases (/etc/pwd.db and /etc/spwd.db).
	sudo pwd_mkdb /etc/master.passwd
	;;
esac
