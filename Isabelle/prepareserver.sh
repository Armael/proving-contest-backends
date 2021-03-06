#!/bin/bash

version=Isabelle2018
graderfolder=/var/lib/isabelle-grader
mkdir -p $graderfolder

isapref=$graderfolder/.isabelle/$version
echo "======================================================"
echo "prepare isabelle preferences in: $isapref"
mkdir -p "$isapref/etc"
mkdir -p "$isapref/heaps"
cp settings "$isapref/etc/settings"

sessions=sessions
echo "======================================================"
echo "build sessions specified in: $sessions"
for sn in `cat $sessions`; do
 echo "---- build $sn -----"
 ~/Isabelle2018/bin/isabelle build -b $sn; 
done 

echo "======================================================"
echo "copy heaps"
cp -R ~/.isabelle/$version/heaps $isapref

echo "======================================================"
echo "copy startserverscript"
cp -R startserverscript $graderfolder


echo "======================================================"
echo "prepare checking files in folder: $graderfolder"
cp OK_Test.thy $graderfolder
cp Defs.thy $graderfolder



echo "======================================================"
echo "setup new network namespace"
./setupnewnamespace.sh
