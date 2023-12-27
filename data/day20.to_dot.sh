#!/bin/bash

inf=$1
outf=${inf%.txt}.dot

echo "digraph {" > $outf
echo "rankdir=BT" >> $outf
echo "broadcaster [shape=invhouse]" >> $outf
echo "rx [shape=Msquare]" >> $outf

# Generate the edges
cat $inf\
  | sed 's/^%//'\
  | sed 's/^&//'\
  >> $outf

# Flip-flops
grep --color=never -F '%' $inf\
  | sed 's/^%\([a-z]\+\) .*$/\1 [shape=parallelogram]/'\
  >> $outf

# Conjunctions
grep --color=never -F '&' $inf\
  | sed 's/^&\([a-z]\+\) .*$/\1 [shape=house]/'\
  >> $outf

echo "}" >> $outf

echo "Output written to $outf"
