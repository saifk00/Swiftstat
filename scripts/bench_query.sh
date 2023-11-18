#!/usr/bin/env bash
NVALS=(2 4 8 16 32)

# prep work: compile everything and get the scc alias
shopt -s expand_aliases
sbt assembly
source env.sh

echo "N,t,mdl,mdlEntity,hardware" > result_query.csv
for n in ${NVALS[@]}; do
  echo "running N=${n}"
  for t in {1..10}; do
    RESULT=$(scc --pgm models/dynamic_32_query_${n}.pgm | sed -n '$p')
    echo "$n,$t,$RESULT" | tee -a result_query.csv
    echo "($t/10)"
  done
  echo "finished N=${n}"
done