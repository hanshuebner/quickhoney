#!/bin/sh

while true
do
	sbcl --dynamic-space-size 800 --no-userinit --load load.lisp --eval '(quickhoney::startup)'
	ps axw | mail -s 'quickhoney crashed' hans.huebner@gmail.com
	sleep 10
done
