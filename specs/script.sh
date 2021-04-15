#!/bin/bash

a=`seq 1 5`
echo "${a}"

for i in $a; do
	echo $i
done
