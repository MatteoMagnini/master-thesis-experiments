# set work directory 
# setwd("~/")

# Get the indices' networks that perform better than the better random walk controller in both tasks.
# Then compute the measures: PI, TE and RTE.
# Save all into files.

source("statistic_functions.R")
library(dplyr)

get_thresholds <- function(files){
  
  get_greatest_fitness <- function(file){
    max(read.table(file, sep = ',')$V1)
  }
  
  lapply(files, get_greatest_fitness)
}

names_to_indices <- function(net_names){
  unlist(lapply(net_names,function(x) as.numeric(gsub(".csv","",gsub(".*experiment","",x)))))
}

get_measures <- function(path, index){
  fit <- read.table(concat(c(path,"/Fitness.csv")))$V1[index]
  se <- read.table(concat(c(path,"/SensorEntropy.csv")))$V1[index]
  pi <- read.table(concat(c(path,"/PredictiveInformation.csv")))$V1[index]
  te <- read.table(concat(c(path,"/TransferEntropy.csv")))$V1[index]
  rte <- read.table(concat(c(path,"/ReverseTransferEntropy.csv")))$V1[index]
  data.frame(fit,se,pi,te,rte)
}

get_networks <- function(file){
  return(read.table(file, sep = ',', stringsAsFactors=FALSE)$V1)
}

################################################################################
################################################################################
################################################################################


get_supernets <- function(measure){
  tasks <- c("/oa","/pf","/pt")
  nodes <- c("/nodes_20","/nodes_50","/nodes_100")
  df <- data.frame(matrix(ncol = 16, nrow = 0))
  chunk <- 5
  
  thresholds_files <- lapply(tasks, function(x) concat(c("data",x,"/random/Fitness.csv")))
  thresholds <- get_thresholds(thresholds_files)
  print(concat(c("Thresholds: ", thresholds, " (oa,pf,pt)")))
  
  for(node in nodes){
    for(task in tasks){
      tmp <- data.frame(matrix(ncol = 16, nrow = 20))
      path <- concat(c("data",task,node))
      top_networks <- get_networks(concat(c(path,measure,".csv")))
      top_indices <- names_to_indices(top_networks)
      measures <- get_measures(path, top_indices)
      task_id <- match(task,tasks) - 1
      tmp[,(chunk*task_id+1):(chunk*(task_id+1))] <- measures
      short_networks <- data.frame(unlist(lapply(top_networks, function(x) substr(x,16,nchar(x)))))
      tmp[,16] <- short_networks
      for(other_task in tasks){
        if(task != other_task){
           path <- concat(c("data",other_task,"/switch",measure,task,node))
           measures <- get_measures(path,1:20)
           task_id <- match(other_task,tasks) - 1
           tmp[,(chunk*task_id+1):(chunk*(task_id+1))] <- measures
        }
      }
      df <- rbind(df,tmp)
    }
  }
  cols <- c("FIT_OA","SE_OA","PI_OA","TE_OA","RTE_OA",
            "FIT_PF","SE_PF","PI_PF","TE_PF","RTE_PF",
            "FIT_PT","SE_PT","PI_PT","TE_PT","RTE_PT",
            "network")
  colnames(df) <- cols
  rows <- ((df$FIT_OA > thresholds[1]) + (df$FIT_PF > thresholds[2]) + (df$FIT_PT > thresholds[3])) > 1
  #df <- df[rows,]
  rownames(df) <- 1:dim(df)[1]
  df
}

measures <- c("/top_fit","/top_se","/top_pi","/top_te","/top_rte")
supernets <- lapply(measures, get_supernets)

for(i in 1:length(measures)){
  write.table(supernets[i], file=concat(c("data/supernets/dummy",measures[i],"_supernets.csv")),
              row.names=FALSE, col.names=TRUE, quote=FALSE, sep=", ")
}

# Just to have a format for save data in latex table.
supernets_latex_format <- lapply(supernets, function(x) {
  
  long_name_to_short_name <- function(name){
    number <- gsub(".csv","",gsub(".*experiment","",name))
    task <- gsub("/.*","",gsub(".*data/","",name))
    nodes <- gsub("/.*","",gsub(".*nodes_","",name))
    concat(c(task,"N",nodes,"E",number))
  }
  
  new_df <- x
  new_df[[(dim(new_df)[2])]] <- unlist(lapply(new_df[,dim(new_df)[2]], function(y) long_name_to_short_name(y)))
  
  new_df %>% 
    mutate_if(is.numeric, round, digits = 4)
  })

for(i in 1:length(net_per_measures)){
  write.table(supernets_latex_format[[i]], file=concat(c(supernets_path,net_per_measures[i],"_supernets_latex.csv")),
              row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")
}

unique_supernets <- unique(unlist(lapply(supernets_latex_format,function(x) x$network)))

dummy <- data.frame()
for(i in supernets_latex_format){
  dummy <- rbind(dummy,i)
}
unique_supernets_latex_format <- dummy[dummy$network %in% unique_supernets,]
unique_supernets_latex_format <- unique_supernets_latex_format[!duplicated(unique_supernets_latex_format),]
length(unique_supernets_latex_format$network)
write.table(unique_supernets_latex_format, file=concat(c(supernets_path,"/supernets_latex.csv")),
            row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")