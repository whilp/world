#!/bin/sh

SRCDIR=/usr/src
PORTSDIR=/usr/ports
CVSROOT=anoncvs@mirror.sg.depaul.edu:/cvs

DIRS="${SRCDIR} ${PORTSDIR}"


CVS=/usr/bin/cvs
MAIL=/usr/bin/mail

for DIR in ${DIRS}; do
    echo "Updating ${DIR} at $(date)."
    cd ${DIR}
    ${CVS} -q up -rOPENBSD_3_8 -Pd 
    echo ""
done

echo ""
echo "Please read /usr/src/Makefile before using the following
procedure to update the system."
echo "  cd /usr/src/sys/arch/i386/conf"
echo "  vi GENERIC"
echo "  /usr/sbin/config GENERIC"
echo "  cd /usr/src/sys/arch/i386/compile/GENERIC"
echo "  make clean && make depend && make"
echo "  cd /usr/src/sys/arch/i386/compile/GENERIC"
echo "  cp /bsd /bsd.old          # Save an old copy of your kernel"
echo "  cp bsd /bsd               # Copy the new kernel into place"
echo "  reboot"
echo "  cd /usr/src"
echo "  rm -r /usr/obj/*"
echo "  make obj && make build"
