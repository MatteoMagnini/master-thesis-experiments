# README #

This readme is in progress.

### What is this repository for? ###

This repository is part of the work for Magnini's thesis: "An information theory analysis of critical Boolean networks as control software for robots".
In this repository there are the source files to execute new experiments or load stored experiments.
There are also files to analyze experiments once executed.
Experiments are on ARGoS simulation software, code is written mostly in Lua.
Information theory analysis is in R.

### Structure ###

* /src -> ARGoS configuration files, controllers and bash scripts to speed-up executions;
* /experiments -> R files to generate graphs and compute metrics;
	* /data -> stored experiments.

### How do I get set up? ###

* To run N new experiment execute "src/run-experiments.sh", use funtion help to see in details;
* To run again a stored experiment execute "src/rerun-single-experiment.sh", use funtion help to see in details;