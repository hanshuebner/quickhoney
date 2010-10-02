#!/bin/sh
#
# Add a watermark to a PDF in probably the ugliest way ever

INPUT_PDF=$1
OUTPUT_PDF=$2
SIG_SVG=$3
NAME=$4

TMPSVG=`mktemp /tmp/watermark.svg.XXXXXX`
TMPPDF=`mktemp /tmp/watermark.XXXXXX`
TMPPDF2=`mktemp /tmp/watermark.XXXXXX`
TMPDATA=`mktemp /tmp/info.XXXXX`

DATE=`date +%F`
WATERMARK="Created for $NAME on $DATE"

# Get size information of source PDF by using 'pdfinfo'
INPUT_SIZE=`pdfinfo "$INPUT_PDF" | grep "Page size" | cut -c17- | sed -e 's/[^0-9 ]//g'`
INPUT_W=`echo "$INPUT_SIZE" | cut -d\  -f1`
INPUT_H=`echo "$INPUT_SIZE" | cut -d\  -f3`

SIG_W=88
SIG_H=28

SIG_X=$((INPUT_W - SIG_W - 20))
SIG_Y=$((INPUT_H - SIG_H - 10))

#echo "INPUT_SIZE: $INPUT_SIZE"
#echo "INPUT_W: $INPUT_W"
#echo "INPUT_H: $INPUT_H"
#echo "WATERMARK: $WATERMARK"

sed -e "s/transform=\"\"/transform=\"translate($SIG_X,$SIG_Y)\"/" \
    -e "s/__W__/$INPUT_W/g" -e "s/__H__/$INPUT_H/g" \
    < "$SIG_SVG" > "$TMPSVG"
#svg2pdf "$TMPSVG" "$TMPPDF"
#pdftk "$INPUT_PDF" stamp "$TMPPDF"  output "$TMPPDF2"
#deillustrate.pl "$TMPPDF2" > "$TMPPDF"
echo "InfoKey: BoughtBy\nInfoValue: $WATERMARK\n" > "$TMPDATA"
cat "$TMPDATA"
pdftk "$INPUT_PDF" update_info "$TMPDATA" output "$OUTPUT_PDF"

rm "$TMPSVG" "$TMPPDF" "$TMPPDF2" "$TMPDATA"

