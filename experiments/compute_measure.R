# set work directory 
# setwd("~/")

# This code computes the specified measure for a given path containing experiment files.
# You have to pass the measure type, the path, and the number of experiment files.
# Then the result is saved in a csv file named in function of the fitness type inside the path.

args = commandArgs(trailingOnly=TRUE)
if (length(args)<3) {
  stop("You have to pass in this order the measure type (se, pi, te, rte), the files' path, the files's number", call.=FALSE)
}

source("statistic_functions.R")
options(digits=10)
THRESHOLD <- 0.1 #Binary threshold that is suitable for all kind of task, if in future tasks will change then this might be modified.

type <- args[1]
path <- args[2]
files_number <- args[3]
base_filename <- paste(path,"/experiment",sep="")
file_format <- ".csv"
files_names <- c()

# If you have already computed the fitness for a subset of experiments you can continue from that point passing the argument "continue".

start_from <- 1
file_name <- ""
switch (type,
  se = {file_name <- concat(c(path, "/SensorEntropy", file_format))},
  pi = {file_name <- concat(c(path, "/PredictiveInformation", file_format))},
  te = {file_name <- concat(c(path, "/TransferEntropy", file_format))},
  rte = {file_name <- concat(c(path, "/ReverseTransferEntropy", file_format))},
  stop("Wrong measure type.")
)
file_already_exsists <- file.exists(file_name)
append <- FALSE
if(length(args)>3){
  if(args[4]=="continue"){
    if (file_already_exsists) {
      append <- TRUE
      myfile <- file(file_name)
      start_from <- length(readLines(myfile)) + 1
      close(myfile)
    }
  }
}

for(i in start_from:files_number){
  name <- concat(c(base_filename,as.character(i), file_format))
  files_names <- c(files_names,name)
}

measure_values <- c()
for(name in files_names){
  data <- load_experiment(name,THRESHOLD)
  input <- data$i
  output <-data$o
  
  m <- 0
  switch (type,
    se = {m <- fast_entropy(input)},
    pi = {m <- fast_predictive_information(input,input)},
    te = {m <- fast_transfer_entropy(input,output)},
    rte = {m <- fast_transfer_entropy(output,input)},
    stop("Wrong measure type.")
  )
  
  print(paste(name, m, sep=":"))
  measure_values <- c(measure_values,m)
}

write.table(measure_values, file = file_name, row.names=FALSE, col.names=FALSE, append=append)
