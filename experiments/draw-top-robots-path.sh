#!/bin/bash

declare -a tasks=("/oa" "/pf" "/pt")
declare -a nodes=("/nodes_20" "/nodes_50" "/nodes_100" "/random")
declare -a measures=("/top_fit" "/top_se" "/top_pi" "/top_te" "/top_rte")
declare -a ms=("se" "pi" "te" "rte")

for task in "${tasks[@]}"; do
  for measure in "${measures[@]}"; do
    for node in "${nodes[@]}"; do
      if [ ${node} == "/random" ]; then
        if [ ${measure} == "/top_fit" ]; then
          echo "./data${task}${node}${measure}.csv"
          Rscript generate_functions_graphs.R "p" "./data${task}${node}${measure}.csv"
        fi
      else
        echo "./data${task}${node}${measure}.csv"
        Rscript generate_functions_graphs.R "p" "./data${task}${node}${measure}.csv"
      fi
    done
  done
done
