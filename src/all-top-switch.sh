#!/bin/bash

declare -a tasks=("/oa" "/pf" "/pt")
declare -a nodes=("/nodes_20" "/nodes_50" "/nodes_100")
declare -a measures=("/top_fit" "/top_se" "/top_pi" "/top_te" "/top_rte") #

for task in "${tasks[@]}"; do
  mkdir "../experiments/data${task}/switch"
  for measure in "${measures[@]}"; do
    mkdir "../experiments/data${task}/switch${measure}"
    other_tasks="${tasks[@]/${task}}"
    for other_task in ${other_tasks}; do
      mkdir "../experiments/data${task}/switch${measure}${other_task}"
      for node in "${nodes[@]}"; do
        mkdir "../experiments/data${task}/switch${measure}${other_task}${node}"
        sh ./run-with-switched-net.sh -a ".${task}-simulation.argos" -s "../experiments/data${task}/switch${measure}${other_task}${node}" -n "../experiments/data${other_task}${node}${measure}.csv"
      done  
    done
  done
done
