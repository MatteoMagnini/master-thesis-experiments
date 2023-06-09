# set work directory 
# setwd("~/")

# This code computes the specified fitness for a given path containing experiment files.
# You have to pass the fitness type, the path, and the number of experiment files.
# Then the result is saved in a file named "Fitness.csv" inside the path.

args = commandArgs(trailingOnly=TRUE)
if (length(args)<3) {
  stop("You have to pass, in this order, the fitness type (oa, pf, pt), the files' path, and the number of experiment files.", call.=FALSE)
}

source("statistic_functions.R")
LIGHT_POSITION <- c(1.5,0,0.5)
binary_threshold <- 0.1

type <- args[1]
path <- args[2]
files_number <- args[3]
base_filename <- paste(path,"/experiment",sep="")
file_format <- ".csv"
files_names <- c()

# If you have already computed the fitness for a subset of experiments you can continue from that point passing the argument "continue".

start_from <- 1
file_name <- concat(c(path, "/Fitness", file_format))
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

#' Compute the obstacle avoidance fitness for a given experiment.
#' 
#' @param file the file name of the experiment.
#' @return the fitness value.
oa_fitness <- function(name){
  row_values <- read.table(name, sep = ';', skip = 1)
  string_positions <- row_values$V3
  positions <- aperm(sapply(string_positions, string_to_vector),c(2,1))
  
  path <- path_lenght(positions)
  max <- max_distance(positions)
  path * max
}

#' Compute the path following fitness for a given experiment.
#' 
#' @param file the file name of the experiment.
#' @return the fitness value.
pf_fitness <- function(name){
  row_values <- read.table(name, sep = ';', skip = 1)
  string_positions <- row_values$V3
  network <- row_values$V1
  row_input <- row_values$V2
  input <- aperm(sapply(row_input, function(v){binarize_vector(string_to_vector(v),binary_threshold)}),c(2,1))
  output_indices <- c(dim(input)[2]+1,dim(input)[2]+2)
  positions <- aperm(sapply(string_positions, string_to_vector),c(2,1))
  output <- aperm(sapply(network, function(x) get_output_values(x,output_indices)),c(2,1))
  
  step <- step_on_circuit(input, output)
  max <- max_distance2(positions, input)
  fit <- step * max 
}

#' Compute the photo-taxis fitness for a given experiment.
#' 
#' @param file the file name of the experiment.
#' @return the fitness value.
pt_fitness <- function(name){
  row_values <- read.table(name, sep = ';', skip = 1)
  string_positions <- row_values$V3
  positions <- aperm(sapply(string_positions, string_to_vector),c(2,1))
  avg_distance_from_light(positions, LIGHT_POSITION)
}

fits <- c()
for(name in files_names){
  fit <- 0
  switch (type,
    oa = {fit <- oa_fitness(name)},
    pf = {fit <- pf_fitness(name)},
    pt = {fit <- pt_fitness(name)},
    stop("Wrong fitness type.")
  )
  print(paste(name, fit, sep=":"))
  fits <- c(fits, fit)
}

write.table(fits, file = file_name, row.names=FALSE, col.names=FALSE, append = append)