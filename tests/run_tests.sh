#!/bin/bash
for test in `cat tests.txt`
do
	echo "Running $test.."
	$CC $test
    bcfile=${test/\.c/".bc"}
    bcfile=sj_$bcfile
    lli $bcfile
    echo ""
done
