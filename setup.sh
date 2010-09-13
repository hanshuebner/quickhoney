#!/bin/sh

echo "Enter the location of the thirdparty directory: "
read THIRDPARTY
echo "Enter the location of the xml catalog: "
read XMLCATALOG
echo "Linking \"$THIRDPARTY/yui\" to website/static/yui..."
ln -s "$THIRDPARTY/yui/build" website/static/yui
echo "Linking \"$XMLCATALOG\" to xml..."
ln -s "$XMLCATALOG" xml
