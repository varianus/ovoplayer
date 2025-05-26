#!/bin/bash
# set -x
#Set path where lazarus is installed
LAZARUS_DIR=~/development/lazarus
CONFIG_PATH=
# 
if [ "$BASE" = "" ]; then
  BASE=$(pwd)
fi   
   
if [ ! "$OS_TARGET" = "" ]; then 
  DC_ARCH="$DC_ARCH --os=$OS_TARGET"
fi
   
if [ ! "$CPU_TARGET" = "" ]; then
 DC_ARCH="$DC_ARCH --cpu=$CPU_TARGET"
fi 

if [ ! "$WIDGETSET_TARGET" = "" ]; then
 DC_ARCH="$DC_ARCH --ws=$WIDGETSET_TARGET"
fi 

NONE=-l
if [ ! "$CONFIG_PATH" = "" ]; then
 PCP=--pcp="$CONFIG_PATH"
fi

# clean build files
rm -Rf $BASE/src/lib/*
rm -Rf $BASE/src/components/lib/*.*
rm -Rf $BASE/tools/ovoplayerctrl/lib/*.*

rm -Rf $BASE/bin/linux
#
cp $BASE/release.cfg  $BASE/extrafpc.cfg
$LAZARUS_DIR/lazbuild -B -r $PCP --build-mode=Release $BASE/src/components/mcaselli.lpk $DC_ARCH
$LAZARUS_DIR/lazbuild -B -r $PCP --build-mode=Release $BASE/src/ovoplayer.lpi $DC_ARCH
$LAZARUS_DIR/lazbuild -B -r $PCP --build-mode=Release $BASE/tools/ovoplayerctrl/ovoplayerctrl.lpi $DC_ARCH
strip --strip-all $BASE/bin/linux/ovoplayer
strip --strip-all $BASE/bin/linux/ovoplayerctrl

echo $NONE > $BASE/extrafpc.cfg
