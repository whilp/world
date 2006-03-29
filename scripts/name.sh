#!/bin/sh
# Set FLAC metadata

# Arrays and things work with OpenBSD pdksh/sh

# Source data
. $PWD/manifest || exit

# Data should look like this:
# ARTIST="Wu-Tang Clan"
# ALBUM="Wu-Tang Forever (Disc 2)"
# GENRE="Hip Hop"
# DATE="1997"
# 
# TRACK[1]="Intro"
# TRACK[2]="Triumph (feat. CappaDonna)"
# TRACK[3]="Impossible (feat. Tekitha)"
# TRACK[4]="Little Ghetto Boys (feat. CappaDonna)"
# TRACK[5]="Deadly Melody (feat. Street Life)"
# TRACK[6]="The City"
# TRACK[7]="The Projects"
# TRACK[8]="Bells of War"
# TRACK[9]="The M.G.M."
# TRACK[10]="Dog Shit"
# TRACK[11]="Duck Seazon"
# TRACK[12]="Hellz Wind Staff (feat. Street Life)"
# TRACK[13]="Heaterz (feat. CappaDonna)"
# TRACK[14]="Black Shampoo"
# TRACK[15]="Second Coming (feat. Tekitha)"
# TRACK[16]="The Closing"

metaflac --remove-all *flac
for FILE in $PWD/*.flac; do
    # This works, assuming you use cdparanoia's default naming
    # scheme: track##.cdda.wav
    echo "Editing .../$(basename $FILE)"
    NUMBER=$(echo $(basename $FILE) | sed -e 's/[^0-9]//g')
    # Remove leading zeros, if any; this causes pdksh to whine about
    # 'bad numbers' when trying to access elements in the array.
    # Harrumph. This isn't a problem for, eg 0[1-7], but errors on
    # eg 08.
    NUMBER=${NUMBER#0}
    echo $NUMBER
    TITLE=${TRACK[$NUMBER]}
    metaflac --set-tag=ALBUM="${ALBUM}" $FILE
    metaflac --set-tag=ARTIST="${ARTIST}" $FILE
    metaflac --set-tag=GENRE="${GENRE}" $FILE
    metaflac --set-tag=DATE="${DATE}" $FILE
    metaflac --set-tag=TRACKNUMBER="${NUMBER}" $FILE
    metaflac --set-tag=TITLE="${TITLE}" $FILE
done
