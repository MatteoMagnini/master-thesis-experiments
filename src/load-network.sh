#!/bin/bash

helpFunction()
{
   echo ""
   echo "Usage: $0 -c controllerfilename -n networkfilename"
   echo -e "\t-c controller file name to modify"
   echo -e "\t-n network file name to load"
   exit 1 # Exit script after printing help
}

while getopts "c:n:" opt
do
   case "$opt" in
      c ) controllerfilename="$OPTARG" ;;
      n ) networkfilename="$OPTARG" ;;
      ? ) helpFunction ;; # Print helpFunction in case parameter is non-existent
   esac
done

if [ -z "$controllerfilename" ] || [ -z "$networkfilename" ]
then
   echo "Some or all of the parameters are empty";
   helpFunction
fi

sed -i "s#local LOAD = .*#local LOAD = true#" $controllerfilename
sed -i "s#local FILENAME = .*#local FILENAME = \"$networkfilename\"##" $controllerfilename