#!/bin/bash

echo "Be sure to have activated the visualization"
helpFunction()
{
   echo ""
   echo "Usage: $0 -a argosfilename -n networkfile"
   echo -e "\t-a argosfilename to run"
   echo -e "\t-n network file to load"
   exit 1 # Exit script after printing help
}

while getopts "a:n:" opt
do
   case "$opt" in
      a ) argosfilename="$OPTARG" ;;
      n ) networkfile="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "$argosfilename" ] || [ -z "$networkfile" ]
then
   echo "Some or all parameters are empty";
   helpFunction
fi

# controller=$(cat ${argosfilename} | grep "<params script=" | head -1 | sed -e 's#<params script=\"##; s#\".*##')
controller="boolean-network-controller.lua"
sh ./load-network.sh -c $controller -n $networkfile

# Can be "oa", "pf" or "pt"
task=${argosfilename:0:2}
sh ./change-task.sh -c $controller -t $task
	
sudo argos3 -c $argosfilename > /dev/null
