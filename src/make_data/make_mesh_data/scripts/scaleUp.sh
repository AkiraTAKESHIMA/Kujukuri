#!/bin/bash

if [ $# != 5 ]; then
  echo "usage: $0 BASINTYPE BASINID RESL_IN RESL_OUT SCALE"
  exit
fi

BASINTYPE=$1
BASINID=$2
RESL_IN=$3
RESL_OUT=$4
SCALE=$5

DIR_DATA="`cd ../../../dat && pwd`"

PATH_CONF=configs/scaleUp/$BASINTYPE/${RESL_IN}_to_${RESL_OUT}/$BASINID.txt
mkdir -p `dirname $PATH_CONF`

echo "config: $PATH_CONF"

DIR_IN=$DIR_DATA/RRI/$BASINTYPE/$RESL_IN/$BASINID
DIR_OUT=$DIR_DATA/RRI/$BASINTYPE/$RESL_OUT/$BASINID

cat << EOF > $PATH_CONF
$DIR_IN/domain.txt
$DIR_IN/elv.txt
$DIR_IN/dir.txt
$DIR_IN/upg.txt
$SCALE
$DIR_OUT/domain.txt
$DIR_OUT/elv.txt
$DIR_OUT/dir.txt
$DIR_OUT/upg.txt
EOF

if [ $RESL_IN == "1sec" ]; then
  if [ $BASINTYPE == "basin" ]; then
    FILE_DOMAIN="$DIR_DATA/J-FlwDir/dat/basin/1sec/domain/$BASINID.txt"
  elif [ $BASINTYPE == "network" ]; then
    FILE_DOMAIN="$DIR_DATA/StrRank/dat/network_mesh/1sec/domain/$BASINID.txt"
  fi

  if [ ! -f $FILE_DOMAIN ]; then
    echo "File not found: $FILE_DOMAIN"
    echo "(current directory: `pwd`)"
    exit 1
  fi

  ln -s $FILE_DOMAIN $DIR_IN/domain.txt
fi


./scripts/scaleUp_at01 $PATH_CONF


if [ $BASINTYPE == "basin" ]; then
  FILE_DOMAIN="$DIR_DATA/J-FlwDir/dat/basin/$RESL_OUT/domain/$BASINID.txt"
elif [ $BASINTYPE == "network" ]; then
  FILE_DOMAIN="$DIR_DATA/StrRank/dat/network_mesh/$RESL_OUT/domain/$BASINID.txt"
fi

mkdir -p `dirname $FILE_DOMAIN`
rm -f $FILE_DOMAIN
ln -s $DIR_OUT/domain.txt $FILE_DOMAIN
