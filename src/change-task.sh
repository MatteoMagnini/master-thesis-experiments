#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -c controller -t task"
   echo -e "\t-c controller file for robot"
   echo -e "\t-t task name, can be 'oa', 'pf' or 'pt'"
   exit 1 # Exit script after printing help
}

while getopts "c:t:" opt
do
   case "$opt" in
      c ) controller="$OPTARG" ;;
      t ) task="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "$controller" ] || [ -z "$task" ]
then
   echo "Some or all parameters are empty";
   helpFunction
fi

case $task in
  oa)
    sed -i "s#local NETWORK_TEST_STEPS = .*#local NETWORK_TEST_STEPS = 2000 --script can change this value#" $controller
	sed -i '/function get_input()/!b;n;creturn argos.get_proximity_values(#network.input_nodes) --script can change this value' $controller
    ;;
  pf)
    sed -i "s#local NETWORK_TEST_STEPS = .*#local NETWORK_TEST_STEPS = 5000 --script can change this value#" $controller
	sed -i '/function get_input()/!b;n;creturn argos.get_base_ground_values(#network.input_nodes) --script can change this value' $controller
    ;;
  pt)    
    sed -i "s#local NETWORK_TEST_STEPS = .*#local NETWORK_TEST_STEPS = 500 --script can change this value#" $controller
	sed -i '/function get_input()/!b;n;creturn argos.get_light_values(#network.input_nodes) --script can change this value' $controller
    ;;
  *)
    echo "Bad argos file name"
	exit 1
	;;
esac
