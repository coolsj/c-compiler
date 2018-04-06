#!/bin/bash
for test in `cat tests.txt`
do
	echo "Running $test.."
	../build/cc $test
    bcfile=${test/\.c/".bc"}
    bcfile=sj_$bcfile
    lli $bcfile
    echo ""
done
