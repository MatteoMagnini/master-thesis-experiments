# set work directory 
# setwd("~/")

# Generate graphs that show the measures obtained by the networks with respect to nodes and random walk controllers.

concat <- function(s){
  
  concat1 <- function(s1,s2){
    paste(s1,s2,sep="")
  }
  
  result <- ""
  for(str in s){
    result <- concat1(result,str)
  }
  return(result)
}

get_measure <- function(file){
  read.table(file)$V1
}

save_measures_boxplots <- function(){
  
  nodes <- c("20","50","100")
  node_names <- c("20"="/nodes_20","50"="/nodes_50","100"="/nodes_100")
  measures <- c("se","pi", "te", "rte")
  measure_names <- c("se"="/SensorEntropy.csv",
                     "pi"="/PredictiveInformation.csv",
                     "te"="/TransferEntropy.csv",
                     "rte"="/ReverseTransferEntropy.csv")
  
  data <- list()
  for(measure in measures){
    for(node in nodes){
      data[[concat(c(measure," ",node))]] <- get_measure(concat(c("data/","pt",node_names[node],measure_names[measure])))
      data[[concat(c(measure," ",node))]] <- 
        data[[concat(c(measure," ",node))]][which(get_measure(concat(c("data/pt",node_names[node],"/Fitness.csv")))>0.272750801316392)]
    }
  }
    
  means <- lapply(data[1:4],mean)
  
  filename <- concat(c("img/pt/boxplot_measures_no_anti_pt_se.png"))
  png(filename = filename, width = 400, height = 600, units = "px")
  bx.p <- boxplot(data[4:12],
                  main = concat(c("Measures distribution for task pt wrt nodes number with no anti pt")),
                  ylab = "Bits",
                  xlab = "# experiments with networks",
                  names = c("SE nodes 20","SE nodes 50","SE nodes 100"),
                  col = c("#0008F0","#0008F0","#0008F0"))
  bx.p$stats[3, ] <- unlist(means)
  bxp(bx.p, add=T, boxfill="transparent", medcol="red", axes=F, outpch = NA, outlty="blank", boxlty="blank", whisklty="blank", staplelty="blank")
  dev.off()
  
  means <- lapply(data[4:12],mean)
  
  filename <- concat(c("img/pt/boxplot_measures_no_anti_pt_no_se.png"))
  png(filename = filename, width = 1000, height = 600, units = "px")
  bx.p <- boxplot(data[4:12],
                  main = concat(c("Measures distribution for task pt wrt nodes number with no anti pt")),
                  ylab = "Bits",
                  xlab = "# experiments with networks",
                  names = c("PI nodes 20","PI nodes 50","PI nodes 100",
                    "TE nodes 20","TE nodes 50","TE nodes 100",
                    "RTE nodes 20","RTE nodes 50","RTE nodes 100"),
                  col = c("#0080F0","#0080F0","#0080F0",
                    "#00F0E8","#00F0E8","#00F0E8",
                    "#00F070","#00F070","#00F070"))
  bx.p$stats[3, ] <- unlist(means)
  bxp(bx.p, add=T, boxfill="transparent", medcol="red", axes=F, outpch = NA, outlty="blank", boxlty="blank", whisklty="blank", staplelty="blank")
  dev.off()
}

save_measures_boxplots()