# set work directory 
# setwd("~/")

# Generate graphics that show the measures obtained by the networks wrt nodes and random walk controllers.

source("statistic_functions.R")

SWITCH <- FALSE
args = commandArgs(trailingOnly=TRUE)
if(length(args)>0) {
  switch (args[1],
    f = {FILTER <- TRUE},
    s = {SWITCH <- TRUE},
    stop("Wrong argument, you can use 'f' (filter by fitness) or 's' (switch nets).")
  )
}
tasks <- c("oa","pf","pt")


get_measure <- function(file){
  read.table(file)$V1
}

save_measures_boxplots <- function(tasks){
  
  nodes <- c("20","50","100")
  node_names <- c("20"="/nodes_20",
                  "50"="/nodes_50",
                  "100"="/nodes_100")
  measures <- c("se","pi","te","rte")
  measure_names <- c("se"="/SensorEntropy.csv",
                     "pi"="/PredictiveInformation.csv",
                     "te"="/TransferEntropy.csv",
                     "rte"="/ReverseTransferEntropy.csv")
  
  for(task in tasks){
    data <- list()
    for(measure in measures){
      for(node in nodes){
        data[[concat(c(measure," ",node))]] <- get_measure(concat(c("data/",task,node_names[node],measure_names[measure])))
      }
    }
    
    means <- lapply(data[1:3],mean)
    filename <- concat(c("img/",task,"/boxplot_measures_se.png"))
    
    png(filename = filename, width = 400, height = 600, units = "px")
    bx.p <- boxplot(data[1:3],
                    ylab = "Bits",
                    names = c("SE 20 nodes","SE 50 nodes","SE 100 nodes"),
                    col = c("#0008F0","#0008F0","#0008F0"))
    bx.p$stats[3, ] <- unlist(means)
    bxp(bx.p, add=T, boxfill="transparent", medcol="red", axes=F, outpch = NA, outlty="blank", boxlty="blank", whisklty="blank", staplelty="blank")
    dev.off()
    
    means <- lapply(data[4:12],mean)
    filename <- concat(c("img/",task,"/boxplot_measures_no_se.png"))
    
    png(filename = filename, width = 1000, height = 600, units = "px")
    bx.p <- boxplot(data[4:12],
                    ylab = "Bits",
                    names = c("PI 20 nodes","PI 50 nodes","PI 100 nodes",
                              "TE 20 nodes","TE 50 nodes","TE 100 nodes",
                              "RTE 20 nodes","RTE 50 nodes","RTE 100 nodes"),
                    col = c("#0080F0","#0080F0","#0080F0",
                            "#00F0E8","#00F0E8","#00F0E8",
                            "#00F070","#00F070","#00F070"))
    bx.p$stats[3, ] <- unlist(means)
    bxp(bx.p, add=T, boxfill="transparent", medcol="red", axes=F, outpch = NA, outlty="blank", boxlty="blank", whisklty="blank", staplelty="blank")
    dev.off()
  }
}

save_measures_boxplots_switch <- function(tasks){
  
  nodes <- c("20","50","100")
  node_names <- c("20"="/nodes_20",
                  "50"="/nodes_50",
                  "100"="/nodes_100")
  measures <- c("se","pi","te","rte")
  measure_names <- c("se"="/SensorEntropy.csv",
                     "pi"="/PredictiveInformation.csv",
                     "te"="/TransferEntropy.csv",
                     "rte"="/ReverseTransferEntropy.csv")
  top_kind <- c("/top_fit","/top_se","/top_pi","/top_te","/top_rte")
  
  for(task in tasks){
    for(other_task in tasks){
        if(task != other_task){
        for(kind in top_kind){
          data <- list()
          for(measure in measures){
            for(node in nodes){
              data[[concat(c(measure," ",node))]] <- get_measure(concat(c("data/",task,"/switch",kind,"/",other_task,node_names[node],measure_names[measure])))
            }
          }
          
          means <- lapply(data[1:12],mean)
          filename <- concat(c("img/",task,"/switch",kind,"/",other_task,"/boxplot_measures.png"))
          png(filename = filename, width = 1500, height = 800, units = "px")
          bx.p <- boxplot(data[1:12],
                          main = concat(c("Measures distribution for task ", task, " using nets from task ", other_task," wrt nodes number")),
                          ylab = "Bits",
                          xlab = concat(c(toString(lengths(data, use.names = FALSE)[1]), " experiments for 20 nodes nets - ",
                                          toString(lengths(data, use.names = FALSE)[2]), " experiments for 50 nodes nets - ",
                                          toString(lengths(data, use.names = FALSE)[3]), " experiments for 100 nodes nets")),
                          names = c("SE nodes 20","SE nodes 50","SE nodes 100",
                                    "PI nodes 20","PI nodes 50","PI nodes 100",
                                    "TE nodes 20","TE nodes 50","TE nodes 100",
                                    "RTE nodes 20","RTE nodes 50","RTE nodes 100"),
                          col = c("#0008F0","#0008F0","#0008F0",
                                  "#0080F0","#0080F0","#0080F0",
                                  "#00F0E8","#00F0E8","#00F0E8",
                                  "#00F070","#00F070","#00F070"))
          bx.p$stats[3, ] <- unlist(means)
          bxp(bx.p, add=T, boxfill="transparent", medcol="red", axes=F, outpch = NA, outlty="blank", boxlty="blank", whisklty="blank", staplelty="blank")
          dev.off()
        }
      }
    }
  }
}

if(SWITCH){
  save_measures_boxplots_switch(tasks)
  } else save_measures_boxplots(tasks)