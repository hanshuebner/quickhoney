#!/bin/sh

PWD=`pwd`
echo "Updating thirdparty from CVS..."
cd ../bknr-svn; svn update thirdparty; cd "$PWD"
echo "Updating bknr-web from git..."
cd ../bknr-web; git pull; cd "$PWD"
echo "Updating bknr-datastore from git..."
cd ../bknr-datastore; git pull; cd "$PWD"
echo "Updating quickhoney from git..."
git pull
