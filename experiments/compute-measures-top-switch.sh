#!/bin/bash

declare -a tasks=("/oa" "/pf" "/pt")
declare -a nodes=("/nodes_20" "/nodes_50" "/nodes_100")
declare -a measures=("/top_fit" "/top_se" "/top_pi" "/top_te" "/top_rte")
declare -a ms=("se" "pi" "te" "rte")

for task in "${tasks[@]}"; do
  if [ ${task} != "/oa" ]; then
    for measure in "${measures[@]}"; do
      other_tasks="${tasks[@]/${task}}"
      for other_task in ${other_tasks}; do
        for node in "${nodes[@]}"; do
          for m in "${ms[@]}"; do
            Rscript compute_measure.R "${m}" "./data${task}/switch${measure}${other_task}${node}" 20
          done
        done  
      done
    done
  fi
done
