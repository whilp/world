#!/bin/sh

# upsys.sh - updates system and third-party software on OpenBSD.

# TODO:
# arg parsing

VERBOSE=1

warn () {
    if [ "${WARNINGS}" -ge $1 ]; then
	echo "=!=> $2"
    fi
}
notify () {
    if [ "${VERBOSE}" -ge $1 ]; then
	echo "===> $2"
    fi
}

SYSCVSTAG=OPENBSD_3_8
SYSCVSROOT=anoncvs@rt.fm:/cvs
SYSFTPSITE=openbsd.rt.fm
SYSBASEDIR=/usr
SYSSRCDIR=${SYSBASEDIR}/src
SYSSRCBAK=${SYSSRCDIR}-old
SYSVERSION=3.8
PKGCVSROOT=anoncvs@anoncvs.netbsd.org:/cvsroot

VERBOSE=1
GETFRESHSRC=1
DATEFMT="+%Y%m%d.%H%M"

backup_src () {
    # Create a backup of the system source
    mv ${SYSSRCDIR} ${SYSSRCBAK}
    mkdir ${SYSSRCDIR}
}

get_fresh_src () {
    # Download and extract fresh system sources; useful on new
    # systems
    for FILE in src sys; do
        TARBALL=${FILE}.tar.gz
        # if [ -f "${SYSBASEDIR}/${TARBALL}" ]; then
	    # notify 1 "Making a backup of ${SYSBASEDIR}/${TARBALL} at ${SYSBASEDIR}/${TARBALL}-old."
	    # mv ${SYSBASEDIR}/${TARBALL} ${SYSBASEDIR}/${TARBALL}-old
        # fi
        notify 1 "Downloading ${TARBALL} from ${SYSFTPSITE}."
        cd ${SYSBASEDIR}
        ftp ftp://${SYSFTPSITE}/pub/OpenBSD/${SYSVERSION}/${TARBALL}
        notify 1 "Extracting ${TARBALL} in ${SYSSRCDIR} -- this may take a while."
        cd ${SYSSRCDIR}
        tar xzf ${SYSBASEDIR}/${TARBALL}
        cd ${SYSBASEDIR}
    done
}

get_fresh_ports () {
    # Download and extract fresh ports; useful on new systems
    TARBALL=ports.tar.gz
    if [ -f "${SYSBASEDIR}/${TARBALL}" ]; then
	notify 1 "Making a backup of ${SYSBASEDIR}/${TARBALL} at ${SYSBASEDIR}/${TARBALL}-old."
	mv ${SYSBASEDIR}/${TARBALL} ${SYSBASEDIR}/${TARBALL}-old
    fi
    notify 1 "Downloading ${TARBALL} from ${SYSFTPSITE}."
    cd ${SYSBASEDIR}
    ftp ftp://${SYSFTPSITE}/pub/OpenBSD/${SYSVERSION}/${TARBALL}
    notify 1 "Extracting ${TARBALL} in ${SYSBASEDIR} -- this may take a while."
    cd ${SYSBASEDIR}
    tar xzf ${TARBALL}
}

update_src_cvs () {
    # Use CVS to update system sources; useful on systems with
    # pre-existing src trees.
    cd ${SYSSRCDIR}
    cvs -d${SYSCVSROOT} -q up -r${SYSCVSTAG} -Pd
}

update_ports_cvs () {
    # Use CVS to update ports sources; useful on systems with
    # pre-existing ports trees.
    cd ${SYSBASEDIR}/ports
    cvs -d${SYSCVSROOT} -q up -r${SYSCVSTAG} -Pd
}

build_src () {
    # Compile the system kernel -- if you change the kernel name,
    # you should already have a file of that name in the path.
    KERNEL=${KERNEL:-GENERIC}
    KERNELLOC=${SYSSRCDIR}/sys/arch/$(machine)
    notify 1 "Configuring the system kernel ${KERNEL}."
    if [ ! -f "${KERNELLOC}/conf/${KERNEL}" ]; then
	warn 1 "Kernel config file ${KERNEL} not found in ${KERNELLLOC}/conf/."
	exit 1
    fi
    cd ${KERNELLOC}/conf
    /usr/sbin/config ${KERNEL}
    cd ${KERNELLOC}/compile/${KERNEL}
    notify 1 "Making kernel ${KERNEL} on $(date "${DATEFMT}")."
    make clean && make depend && make
    # TODO: Check to see how that went...
    notify 1 "Installing kernel ${KERNEL}."
    make install
}

build_world () {
    # Compile the userland binaries; this should be done after
    # building a new kernel.
    notify 1 "Cleaning old objects in ${SYSBASEDIR}/obj."
    rm -rf ${SYSBASEDIR}/obj/* 2>/dev/null
    cd ${SYSSRCDIR}
    notify 1 "Building objects in ${SYSSRCDIR} on $(date "${DATEFMT}")."
    make obj
    notify 1 "Creating distrib-dirs."
    cd ${SYSSRCDIR}/etc
    DESTDIR=/ make distrib-dirs 
    cd ${SYSSRCDIR}
    notify 1 "Building userland binaries in ${SYSSRCDIR} -- this may take a while."
    make build 
}

NIGHTLY=

if [ "$#" -lt 1 ]; then
    warn 1 "Missing arguments."
    usage
    exit 1
fi

# Parse args
while getopts "pnbf" ARG; do
    case ${ARG} in
	n) NIGHTLY=1 ;;
	f) FRESH_SRC=1 ;;
	b) BUILD_SRC=1 ;;
	p) FETCH_PORTS=1 ;;
    esac
done

if [ ${NIGHTLY} ]; then
    # For nightly CVS use:
    # notify 1 "Making a backup of ${SYSSRCDIR} in ${SYSSRCBAK}."
    # backup_src
    notify 1 "Updating system source tree in ${SYSSRCDIR} at $(date "${DATEFMT}")." 
    update_src_cvs
    notify 1 "Updating ports tree in ${SYSSRCDIR} at $(date "${DATEFMT}")." 
    update_ports_cvs
    exit 0
fi

if [ ${FRESH_SRC} ]; then
    # For new systems:
    get_fresh_src
    notify 1 "Updating system source tree in ${SYSSRCDIR} at $(date "${DATEFMT}")." 
    update_src_cvs
    notify 1 "Building system source at $(date "${DATEFMT}")."
    build_src
    warn 1 "Build not complete!"
    notify 1 "You should now reboot this machine; after the reboot, run this program again"
    notify 1 "with the TODO flags to build the userland."
fi

if [ ${BUILD_SRC} ]; then
    # For new systems after a reboot:
    notify 1 "Building userland source at $(date "${DATEFMT}")."
    build_world
fi

if [ ${FETCH_PORTS} ]; then
    notify 1 "Downloading new ports."
    get_fresh_ports
    notify 1 "Updating ports tree."
    update_ports_cvs
fi
