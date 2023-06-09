# set work directory 
# setwd("~/")

# Generate graphics that show the fitness obtained by the networks with respect to nodes and random walk controllers.

source("statistic_functions.R")

SWITCH <- FALSE
args = commandArgs(trailingOnly=TRUE)
if(length(args)>0 && args[1]=="s") {
  SWITCH <- TRUE
} else stop("Wrong argument, you can use 's' (switch nets).")
tasks <- c("oa","pf","pt")

get_measure <- function(file){
  return(read.table(file, sep = ',', stringsAsFactors=FALSE)$V1)
}

#' Given a set of tasks, for each of them saves a boxplot of the fitness of the controllers populations.
#' 
#' @param tasks a set of tasks.
save_fitness_boxplots <- function(tasks){
  
  nodes <- c("20","50","100","random")
  node_names <- c("20"="/nodes_20","50"="/nodes_50","100"="/nodes_100","random"="/random")
  
  for(task in tasks){
    data <-  list()
    for(node in nodes){
      data[[node]] <- get_measure(concat(c("data/",task,node_names[node],"/Fitness.csv")))
    }
    
    means <- lapply(data,mean)
    
    file_name <- concat(c("img/",task,"/boxplot_fitness.png"))
    title <- concat(c("Fitness distribution for task ", task, " wrt nodes number"))
    second_title <- "1000 experiments with networks and random walk controller"
    names <- c("20 nodes", "50 nodes", "100 nodes", "random")
    
    png(filename = file_name, width = 800, height = 500, units = "px")
    bx.p <- boxplot(data,
                    ylab = "Objective function",
                    names = names,
                    col = c("#0008F0", "#0008F0", "#0008F0", "#0008F0"))
    bx.p$stats[3, ] <- unlist(means)
    bxp(bx.p, add=T, boxfill="transparent", medcol="red", axes=F, outpch = NA, outlty="blank", boxlty="blank", whisklty="blank", staplelty="blank")
    dev.off()
  }
}

#' Given a set of tasks, for each of them saves a boxplot of the fitness of the controllers populations.
#' In this case distributions are grouped by the first execution task.
#' 
#' @param tasks a set of tasks.
save_fitness_boxplots_switch <- function(tasks){
  
  names_to_indices <- function(net_names){
    unlist(lapply(net_names,function(x) as.numeric(gsub(".csv","",gsub(".*experiment","",x)))))
  }
  
  top_kind <- c("/top_fit","/top_se","/top_pi","/top_te","/top_rte")
  nodes <- c("20","50","100","random")
  node_names <- c("20"="/nodes_20","50"="/nodes_50","100"="/nodes_100","random"="/random")
  
  for(task in tasks){
    for(kind in top_kind){
      data <- list()
      for(node in nodes){
        for(other_task in tasks){
          if(node != "random"){
            if(task == other_task){
              indices <- names_to_indices(get_measure(concat(c("data/",task,node_names[node],kind,".csv"))))
              data[[concat(c(other_task," ",node))]] <- get_measure(concat(c("data/",task,node_names[node],"/Fitness.csv")))[indices]
            } else {
              data[[concat(c(other_task," ",node))]] <- get_measure(concat(c("data/",task,"/switch",kind,"/",other_task,node_names[node],"/Fitness.csv")))
            }
          } else {
            random <- get_measure(concat(c("data/",task,node_names[node],"/Fitness.csv")))
            data[[node]] <- random[rev(tail(order(random),lengths(data)[1]))]
          }
        }
      }
    
      means <- lapply(data,mean)
      
      dir.create(concat(c("img/",task,"/switch")), showWarnings = FALSE)
      dir.create(concat(c("img/",task,"/switch",kind)), showWarnings = FALSE)
      tasks_string <- "oa, pf and pt"
      file_name <- concat(c("img/",task,"/switch/boxplot_fitness_",substring(kind, 2),".png"))
      title <- concat(c("Fitness distribution for task ", task, " using top ",lengths(data)[1],
                        " nets by ", substring(kind, 6)," from all task ", tasks_string, " wrt nodes number"))
      second_title <- concat(c("Top ", lengths(data)[1] ," experiments with networks and top ", lengths(data)[1], " random walk controller"))
      names <- c(concat(c("20 nodes ", tasks[1])),
                 concat(c("20 nodes ", tasks[2])),
                 concat(c("20 nodes ", tasks[3])),
                 concat(c("50 nodes ", tasks[1])),
                 concat(c("50 nodes ", tasks[2])),
                 concat(c("50 nodes ", tasks[3])),
                 concat(c("100 nodes ", tasks[1])),
                 concat(c("100 nodes ", tasks[2])),
                 concat(c("100 nodes ", tasks[3])),
                 "random")
      
      png(filename = file_name, width = 1200, height = 600, units = "px")
      bx.p <- boxplot(data,
                      main = title,
                      ylab = "Fitness",
                      xlab = second_title,
                      names = names,
                      col = c("#0008F0","#0008F0","#0008F0","#0080F0","#0080F0","#0080F0","#00F0E8","#00F0E8","#00F0E8","#00F070"))
      bx.p$stats[3, ] <- unlist(means)
      bxp(bx.p, add=T, boxfill="transparent", medcol="red", axes=F, outpch = NA, outlty="blank", boxlty="blank", whisklty="blank", staplelty="blank")
      dev.off()
    }
  }
}

if(SWITCH){
  save_fitness_boxplots_switch(tasks)
} else save_fitness_boxplots(tasks)