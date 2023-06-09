#install.packages("ggplot2")
#install.packages("grid")
library("ggplot2")
library("grid")

source("statistic_functions.R")

draw_K <- function() {
  critical_K <- function(p) 1/(2*p*(1-p))
  graphics.off()
  png(filename = "img/critical-K.png", width = 800, height = 500, units = "px")
  
  ggplot(data.frame(x=c(0.05, 0.95)), aes(x=x)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_function(fun=critical_K, size=2) +
    geom_text(x=0.1, y=3, label="Order", size=14) +
    geom_text(x=0.5, y=7, label="Chaos", size=14) +
    labs(x="p",y="K", title="Function for computing the critical value of K given p", size=10)
  
  dev.off()
}

draw_entropy <- function(){
  one_variable_entropy <- function(p) - (p * my_log2(p) + (1-p) * my_log2(1-p))
  graphics.off()
  png(filename = "img/entropy.png", width = 800, height = 500, units = "px")
  
  ggplot(data.frame(x=c(0.01, 0.99)), aes(x=x)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    stat_function(fun=one_variable_entropy, size=2) +
    labs(x="p",y="bits", title="Entropy of a Boolean variable X", size=12)
  
  dev.off()
}

draw_paths <- function(name){
  
  task <- gsub("/node.*csv","",gsub("data/","",name))
  task <- gsub("/random.*csv","",task)
  task <- substr(task,3,5)
  
  get_path <- function(name){
    row_values <- read.table(name, sep = ';', skip = 1)
    string_positions <- row_values$V3
    positions <- data.frame(aperm(sapply(string_positions, string_to_vector),c(2,1)),gl(length(string_positions)/2,2))
    colnames(positions) <- c("x","y","group")
    positions
  }
  
  get_best_paths <- function(name){
    
    names_to_indices <- function(net_names) unlist(lapply(net_names,function(x) as.numeric(gsub(".csv","",gsub(".*experiment","",x)))))
    get_networks <- function(file) read.table(file, sep = ',', stringsAsFactors=FALSE)$V1
    top_networks <- get_networks(name)
    top_indices <- names_to_indices(top_networks)
    dir <- gsub("/top_.*csv","",name)
    lapply(top_indices, function(x) get_path(concat(c(dir,"/experiment",x,".csv"))))
  }
  
  #External bourder
  elements <- data.frame(c(-3.05,-3.05,-3.05,2.95),
                         c(3.05,3.05,-2.95,3.05),
                         c(-3.05,2.95,-3.05,-3.05),
                         c(-2.95,3.05,3.05,3.05),
                         1:4)
  colnames(elements) <- c("xmin","xmax","ymin","ymax","id")
  full_task_name <- ""
  switch (task,
    oa = {obstacles <- data.frame(c(2,-0.5,-3,-0.5,1.25,-2.25,1.25,-2.25,-0.75),
                                  c(3,0.5,-2,0.5,2.25,-1.25,2.25,-1.25,0.75),
                                  c(-0.5,2,-0.5,-3,1.25,-2.25,-2.25,1.25,-0.75),
                                  c(0.5,3,0.5,-2,2.25,-1.25,-1.25,2.25,0.75),
                                  5:13)
      colnames(obstacles) <- c("xmin","xmax","ymin","ymax","id")
      elements <- rbind(elements,obstacles)
      full_task_name <- "obstacle avoidance"
    },
    pf = {full_task_name <- "path following"},
    pt = {elements <- data.frame(c(-4.05,-4.05,-4.05,3.95),
                                 c(4.05,4.05,-3.95,4.05),
                                 c(-4.05,3.95,-4.05,-4.05),
                                 c(-3.95,4.05,4.05,4.05),
                                 1:4)
      colnames(elements) <- c("xmin","xmax","ymin","ymax","id")
      full_task_name <- "photo-taxis"}
  )
  paths <- get_best_paths(name)
  df <- data.frame(matrix(ncol=3,nrow=0))
  colnames(df) <- c("x","y","group")
  for(path in paths) df <- rbind(df,path)
  df$group <- gl(length(df$group)/2,2)
  
  filename <- concat(c(gsub("data","img",gsub(".csv","",name)),"_paths.png"))
  title <- ""
  #title <- concat(c("Robots path during ", full_task_name," experiments"))
  
  plot <- ggplot(df) +
    geom_rect(data=elements, inherit.aes=FALSE,
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,
                  group=id), alpha=0.8) +
    geom_line(aes(x=x, y=y, group=group)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(x="X",y="Y", title=title, size=14)
  
  if (task=="pt") plot <- plot +
    annotation_custom(grob=circleGrob(r=unit(1,"npc"),gp = gpar(fill = "red")), xmin=1.47, xmax=1.53, ymin=-0.03, ymax=0.03)
  ggsave(filename, plot = plot, width = 21.16, height = 21.16, units = "cm", dpi = 72)
}

#draw_paths("data/pt/nodes_20/top_fit.csv")

args = commandArgs(trailingOnly=TRUE)
if(length(args)>0) {
  switch (args[1],
          k = {draw_K()},
          e = {draw_entropy()},
          p = {
            if(length(args)>1) {
              draw_paths(args[2])
            } else {
              stop("You have to specify the top networks file to analyse.")
            }
          },
          stop("Wrong argument, you can use 'k' (K - p relation), 'e' (entropy), 'p' (robots path).")
  )
}
