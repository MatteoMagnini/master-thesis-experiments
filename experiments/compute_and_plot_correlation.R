# set work directory 
# setwd("~/")

print("Importing some libraries...")
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(broom))
suppressMessages(library(ggplot2))
suppressMessages(library("scatterplot3d"))
suppressMessages(library(rgl))
suppressMessages(library(magick))
print("... libraries imported.")

source("statistic_functions.R")

#' Load the measures of: fitness, predictive information, transfer entropy and reverse transfer entropy.
#' 
#' @param name the name path where the measures files are stored.
#' @param filter specify if you want discard some experiment based on fitness.
#' @param threshold the value to surpass to be kept.
#' @return a dataframe with fitness, predictive information, transfer entropy and reverse transfer entropy.
load_measures <- function(name, filter=FALSE, threshold=0){
  
  file_fitness <- paste(name,"/Fitness.csv",sep="")
  file_se <- paste(name,"/SensorEntropy.csv",sep="")
  file_pi <- paste(name,"/PredictiveInformation.csv",sep="")
  file_te <- paste(name,"/TransferEntropy.csv",sep="")
  file_ter <- paste(name,"/ReverseTransferEntropy.csv",sep="")
  
  fit <- read.table(file_fitness, sep = ',')$V1
  se <- read.table(file_se, sep = ',')$V1
  pi <- read.table(file_pi, sep = ',')$V1
  te <- read.table(file_te, sep = ',')$V1
  rte <- read.table(file_ter, sep = ',')$V1
  
  if(filter){
    ids <- which(fit>threshold)
    fit <- fit[ids]
    se <- se[ids]
    pi <- pi[ids]
    te <- te[ids]
    rte <- rte[ids]
  }
  
  ordered_ids <- order(fit)
  
  df <- data.frame(fit[ordered_ids],se[ordered_ids],pi[ordered_ids],te[ordered_ids],rte[ordered_ids])
  colnames(df) <- c("fit", "se", "pi", "te", "rte")
  df
}

#################################################################

#' Compute Pearson's correlation of the measures.
#' 
#' @param df the dataframe with the measures.
#' @return a dataframe with measure names, first row, correlations, second row, and the p-values, third row.
compute_correlations <- function(df){
  test_se <- cor.test(df$fit, df$se, method=c("pearson", "kendall", "spearman"))
  test_pi <- cor.test(df$fit, df$pi, method=c("pearson", "kendall", "spearman"))
  test_te <- cor.test(df$fit, df$te, method=c("pearson", "kendall", "spearman"))
  test_rte <- cor.test(df$fit, df$rte, method=c("pearson", "kendall", "spearman"))
  
  correlations <- data.frame(c("Sensor Entropy", test_se$estimate, test_se$p.value),
                             c("Predictive Information", test_pi$estimate, test_pi$p.value),
                             c("Transfer Entropy", test_te$estimate, test_te$p.value),
                             c("Reverse Transfer Entropy", test_rte$estimate, test_rte$p.value))
  correlations <- unname(correlations)
  correlations
}

#################################################################

#' Draw a 3d scatter plot.
#' 
#' Draw a 3d scatter plot with the following dimensions:
#' - x axis = predictive information;
#' - y axis = transfer entropy;
#' - z axis = reverse transfer entropy.
#' Magic numbers are optimized for a good looking plot with width = 1000px and height = 714px.
#' 
#' @param df the dataframe of the points (names of the cols: fits, pis, tes, rtes).
#' @param title_name the title of the plot.
#' @param img_name the name of the img file to store.
plot_3d_points <- function(df,title_name,img_name) {
  
  myColorRamp <- function(colors, values) {
    v <- (values - min(values))/diff(range(values))
    x <- colorRamp(colors)(v)
    rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
  }
  
  color_names <- c("lightblue","blue","red")
  cols <- myColorRamp(color_names, df$fit)
  cols_in_legend <- myColorRamp(color_names, seq(min(df$fit), max(df$fit), length = 1000))
  
  graphics.off()
  png(filename = img_name, width = 1000, height = 714, units = "px")
  layout(cbind(1:2, 1:2), respect = FALSE, heights = c(3, 1))
  s3d <- scatterplot3d(df[,2:4], angle = 55, type="h", pch = 16, color = cols, box=FALSE,
                       mar = c(3,4,0,4),
                       xlim=c(0,1.5),
                       ylim=c(0,1.5),
                       zlim=c(0,1.0),
                       xlab = "Predictive Information (bit)",
                       ylab = "Transfer Entropy (bit)",
                       zlab = "Reverse Transfer Entropy (bit)")
  par(cex.main = 2.0)
  title(title_name, line = -4)
  par(mar=c(0,4,0,4),mgp=c(5,1,0), cex.main = 1.2)
  plot(seq(min(df$fit), max(df$fit), length = 1000), rep(0, 1000), pch = 15,
       axes = FALSE, xlab = "", ylab = "", col = cols_in_legend)
  title("color code of variable Fitness", line = -4)
  axis(1, at = floor(seq(min(df$fit), max(df$fit), max(df$fit)/5)*1000)/1000, line=-5)
  dev.off()
}

#################################################################

#' Draw a 2d correlation between fitness and a measure.
#' 
#' @param measures the dataframe of measures.
#' @param measure_name the name of the measure
#' @param title the title of the plot.
#' @param img_name of the file to store.
#' @param reg_color color of the regression line.
plot_correlation <- function(measures, measure_name, title, img_name, reg_color){
  
  extended_name <- ""
  switch (measure_name,
    se = {extended_name <- "SE"},
    pi = {extended_name <- "PI"},
    te = {extended_name <- "TE"},
    rte = {extended_name <- "RTE"}
  )
  
  dummy <- measures[,c("fit",measure_name)]
  colnames(dummy) <- c("fit","measure")
    
  graphics.off()
  png(filename = img_name, width = 800, height = 500, units = "px")
  
  img <- ggplot(dummy, aes(x=measure, y=fit)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_point() +
    geom_smooth() +
    labs(x=concat(c(extended_name," (bits)")),y="Objective function")#title=title
  print(img)
  
  dev.off()
  
}

#################################################################

#' Compute the Pearson's correlation between the objective function and the metrics (SE, PI, TE, RTE).
#' Also, for each correlation saves the corresponding plot.
#' 
#' @param task the name of the task.
#' @param nodes_number the number of nodes of the network.
compute_and_plot_correlation <- function(task,nodes_number) {
  
  full_task_name <- ""
  switch (task,
          oa = {full_task_name <- "Obstacle Avoidance"},
          pf = {full_task_name <- "Path Following"},
          pt = {full_task_name <- "Photo-taxis"},
          stop("Wrong task name. Task must be 'oa', 'pf', or 'pt'.", call.=FALSE)
  )
  img_path <- concat(c("img/", task, "/nodes_", nodes_number))
  data_path <- concat(c("data/", task, "/nodes_", nodes_number))
  correlations <- compute_correlations(measures)
  print(correlations, row.names = FALSE)
  
  filename <- paste(data_path,"/correlations-2.csv",sep="")
  write.table(correlations, file=filename, row.names=FALSE,col.names=FALSE,quote=FALSE)
  
  print("Saving images...")
  measure_names <- c("se","pi","te","rte")
  
  img_names <- c(paste(img_path,"/fit_se.png",sep=""),
                 paste(img_path,"/fit_pi.png",sep=""),
                 paste(img_path,"/fit_te.png",sep=""),
                 paste(img_path,"/fit_rte.png",sep=""))
  
  for(i in 1:length(measure_names)){
    measure_name <- measure_names[i]
    title <- concat(c("Fit over ", measure_name, " in ", task, " task with ", nodes_number,
                    " nodes networks (", as.character(lengths(measures)[1]), " experiments each)"))
    print(paste("saving image ", measure_name, sep=''))
    plot_correlation(measures, measure_name, title, img_names[i],"#0008F0")
  }
  
  print("saving image 3d")
  img_name_3d <- paste(img_path,"/3d.png",sep="")
  title_name <- concat(c(full_task_name, ": ",dim(measures)[1]," networks with ", nodes_number," nodes"))
  plot_3d_points(measures[,c("fit","pi","te","rte")], title_name, img_name_3d)
}

tasks <- c("oa","pf","pt")
nodes <- c("20","50","100")
for(task in tasks){
  for(node in nodes){
    compute_and_plot_correlation(task,node)
  }
}