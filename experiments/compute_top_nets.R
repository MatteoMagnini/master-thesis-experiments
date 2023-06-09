# Run this script to save a boxplot of the fitnesses of a specific task.

args = commandArgs(trailingOnly=TRUE)
if (length(args)<1) {
  stop("You have to specify the task name (oa, pf, pt).
       You optionally can specify how many top networks take into account, default is 20.", call.=FALSE)
}

source("statistic_functions.R")

task <- args[1]

switch (task,
  oa = {full_task_name <- "Obstacle Avoidance"},
  pf = {full_task_name <- "Path Following"},
  pt = {full_task_name <- "Photo-taxis"},
  stop("Wrong task name. Task must be 'oa', 'pf', or 'pt'.", call.=FALSE)
)

top <- 20
if(length(args)>1) top <- args[2]
end_folders <- c("/nodes_20","/nodes_50","/nodes_100","/random")
measures <- c("/Fitness.csv",
              "/SensorEntropy.csv",
              "/PredictiveInformation.csv",
              "/TransferEntropy.csv",
              "/ReverseTransferEntropy.csv")
task_path <- paste("data/", task, sep="")
end_path <- lapply(end_folders, function(x) paste(task_path, x, sep=""))

load_specific_measure <- function(path, measure){
  file <- paste(path,measure,sep="")
  read.table(file, sep = ',')$V1
}

#' For each metric saves a file of the top TOP networks that obtained the highest values with respect to the metric.
#' 
#' @param top the number of networks to take into account.
save_top_network_per_measure <- function(top){
  
  for(node in end_path[1:3]){
    top_file_names <- c("/top_fit.csv","/top_se.csv","/top_pi.csv","/top_te.csv","/top_rte.csv")
    for(measure in measures){
      measure_values <- load_specific_measure(node,measure)
      ordered_ids <- order(measure_values)
      topN <- rev(tail(ordered_ids,top))
      relative_folder <- paste("../experiments/", node, sep="")
      
      controllers <- c()
      for(id in 1:top){
        exp <- paste(paste("/experiment", topN[id], sep=""), ".csv", sep="")
        controller <- paste(relative_folder, exp, sep="")
        controllers <- c(controllers, controller)
      }
      
      file_controllers <- paste(node,top_file_names[1],sep="")
      write.table(controllers, file=file_controllers, row.names=FALSE, col.names=FALSE, quote=FALSE)
      
      if(length(top_file_names) > 1) top_file_names <- top_file_names[2:length(top_file_names)]
    }
  }
  
  random <- end_path[4]
  measure_values <- load_specific_measure(random,measures[1])
  ordered_ids <- order(measure_values)
  topN <- rev(tail(ordered_ids,top))
  relative_folder <- paste("../experiments/", random, sep="")
  top_fit <- "/top_fit.csv"
  
  controllers <- c()
  for(id in 1:top){
    exp <- paste(paste("/experiment", topN[id], sep=""), ".csv", sep="")
    controller <- paste(relative_folder, exp, sep="")
    controllers <- c(controllers, controller)
  }
  
  file_controllers <- paste(random,top_fit,sep="")
  write.table(controllers, file=file_controllers, row.names=FALSE, col.names=FALSE, quote=FALSE)
  
}

save_top_network_per_measure(top)