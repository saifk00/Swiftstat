#!/usr/bin/env bash
NVALS=(512) #(0 1 2 4 8 16 32 64 128 256 512)

# prep work: compile everything and get the swiftstatcc alias
shopt -s expand_aliases
sbt assembly
source env.sh

echo "N,t,mdl,mdlEntity,hardware" > result.csv
for n in ${NVALS[@]}; do
  echo "running N=${n}"
  for t in {1..10}; do
    RESULT=$(swiftstatcc --pgm models/dynamic_${n}.pgm | sed -n '$p')
    echo "$n,$t,$RESULT" | tee -a result.csv
    echo "($t/10)"
  done
  echo "finished N=${n}"
done