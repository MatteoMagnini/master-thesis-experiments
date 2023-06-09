#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -a argosfilename -s path -n networkfile"
   echo -e "\t-a argosfilename to run"
   echo -e "\t-s path to save the results"
   echo -e "\t-n network file name"
   exit 1 # Exit script after printing help
}

while getopts "a:s:n:" opt
do
   case "$opt" in
      a ) argosfilename="$OPTARG" ;;
      s ) path="$OPTARG" ;;
      n ) networkfile="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "$argosfilename" ] || [ -z "$path" ] || [ -z "$networkfile" ]
then
   echo "Some or all of the parameters are empty";
   helpFunction
fi

filename="$path/experiment"
existingFiles=$(ls -dq ${filename}* 2>/dev/null | wc -l)
# controller=$(cat ${argosfilename} | grep "<params script=" | head -1 | sed -e 's#<params script=\"##; s#\".*##')
controller="boolean-network-controller.lua"
i=1


# Can be "oa", "pf" or "pt"
task=${argosfilename:0:2}
sh ./change-task.sh -c $controller -t $task

while read network; do
  net=$(echo $network | tr -d '\r')
  sh ./load-network.sh -c $controller -n $net
  argos3 -c $argosfilename | grep -v 'INFO' | grep -v "0m" > "${filename}$(($existingFiles + $i)).csv"
  a='Simulation'
  b='ended'
  c="${a} $(($existingFiles + $i)) ${b}"
  echo "${c}"
  i=$((i+1))
done < $networkfile