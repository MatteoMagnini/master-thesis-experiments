#!/bin/bash

echo "Be sure to have disable the visualization"
echo "Consider editing this file adding a sleep to avoid having the same initialization seed in controllers if computation is fast."
helpFunction()
{
   echo ""
   echo "Usage: $0 -a argosfilename -s path -n experiments"
   echo -e "\t-a argosfilename to run"
   echo -e "\t-s path to save the results"
   echo -e "\t-n number of experiment (suggested value >= 30)"
   exit 1 # Exit script after printing help
}

while getopts "a:s:n:" opt
do
   case "$opt" in
      a ) argosfilename="$OPTARG" ;;
      s ) path="$OPTARG" ;;
      n ) experiments="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "$argosfilename" ] || [ -z "$path" ] || [ -z "$experiments" ]
then
   echo "Some or all of the parameters are empty";
   helpFunction
fi

filename="$path/experiment"
existingFiles=$(ls -dq ${filename}* 2>/dev/null | wc -l)

# Can be "oa", "pf" or "pt"
task=${argosfilename:0:2}
sh ./change-task.sh -c $controller -t $task

for i in $(seq "$experiments")
do
  argos3 -c $argosfilename | grep -v 'INFO' | grep -v "0m" > "${filename}$(($existingFiles + $i)).csv"
  a='Simulation'
  b='ended'
  c="${a} $(($existingFiles + $i)) ${b}"
  echo "${c}"
  # sleep 0.1
done
