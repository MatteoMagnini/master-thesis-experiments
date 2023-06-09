# set work directory 
# setwd("~/")

# Compute Wilcoxon test between different nodes networks on fitness values.

source("statistic_functions.R")

tasks <- c("oa","pf","pt")
nodes <- c(20,50,100)
nodes_names <- c("20"="/nodes_20","50"="/nodes_50","100"="/nodes_100")
fit_file <- "Fitness.csv"

get_fitness <- function(path){
  read.table(concat(c(path,"/",fit_file)), sep = ',', stringsAsFactors=FALSE)$V1
}

results <- data.frame(matrix(nrow = 9, ncol = 3))
colnames(results) <- c("data","p-value","reject-H0")
row <- 1
for(task in tasks){
  for(node1 in nodes){
    for(node2 in nodes){
      if(node1 < node2){
        f1 <- get_fitness(concat(c("data/",task,nodes_names[as.character(node1)])))
        f2 <- get_fitness(concat(c("data/",task,nodes_names[as.character(node2)])))
        w <- wilcox.test(f1,f2,paired = FALSE,conf.level = 0.99)
        results[row,] <- c(concat(c(task,":",node1,"-",node2)),w$p.value, w$p.value < 0.01)
        row <- row + 1
      }
    }
  }
}

write.table(results, file="data/wilcoxon.csv",
            row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t")