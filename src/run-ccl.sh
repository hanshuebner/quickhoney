#!/bin/sh

while true
do
	ccl --no-init --load load.lisp --eval '(quickhoney::startup)'
	ps axw | mail -s 'quickhoney crashed' wesen@ruinwesen.com
	sleep 10
done
