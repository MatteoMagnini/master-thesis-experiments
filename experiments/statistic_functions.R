#########################
### Utility functions ###
#########################

#' Concatenate more string in one.
#' 
#' @param s an array of string.
#' @return the concatenated string with no separator.
concat <- function(s){
  
  concat1 <- function(s1,s2){
    paste(s1,s2,sep="")
  }
  
  result <- ""
  for(str in s){
    result <- concat1(result,str)
  }
  result
}

#' Converting a string format vector into a numeric vector.
#' 
#' @param string the string vector, must be in the form { #, #, ..., # }.
#' @return the numeric vector.
string_to_vector <- function(string){
  no_left_par <- substring(string, 3)
  no_right_par <- substr(no_left_par, 1, nchar(no_left_par)-1)
  inputs_as_char <- strsplit(no_right_par,",")
  inputs_as_number <- c()
  for (char in inputs_as_char) {
    inputs_as_number <- c(inputs_as_number,as.double(char))
  }
  inputs_as_number
}

binarize_vector <- function(vector,binary_threshold) sapply(vector, function(v){if(v<binary_threshold) 0 else 1})

get_output_values <- function(network_state, output_indices){
  state <- string_to_vector(network_state)
  state[output_indices[1]:output_indices[2]]
}

count <- function(element, data){
  counter <- 0
  if (dim(data)[2] == dim(array(element))){
    for(row in 1:dim(data)[1]){
      if(all(data[row,] == element)) {
        counter <- counter + 1
      }
    }
  }
  counter
}

my_div <- function(n,d){
  if(d>0) n/d
  else 0
}

my_log2 <- function(x) {
  if(x>0) log(x,2)
  else 0
}

#' Get a dataframe with input (i) and output (o) for every step of an experiment.
#' For the moment both input and output must be vectors of more than one element.
#' 
#' @param name the file name of the experiment.
#' @param threshold the threshold to binarize not binary sensor values, default is 0.1.
#' @param outputs number of outputs, defalut is 2.
#' @return the dataframe.
load_experiment <- function(name, threshold=0.1, outputs=2){
  row_values <- read.table(name, sep = ';', skip = 1)
  network <- row_values$V1
  row_input <- row_values$V2
  
  input <- sapply(row_input, function(v){binarize_vector(string_to_vector(v),threshold)})
  input <- aperm(input,c(2,1))
  output_indices <- c(dim(input)[2] + 1, dim(input)[2] + outputs)
  output <- aperm(sapply(network, function(x){get_output_values(x,output_indices)}),c(2,1))
  
  list(i=input,o=output)
}


################
### Geometry ###
################

#' 2D Euclidean distance between two points.
#' 
#' @param v1 first point.
#' @param v2 second point.
#' @return the distance between the two points.
vector_distance <- function(v1,v2) (sqrt(((v1[1] - v2[1]) ** 2) + ((v1[2] - v2[2]) ** 2)))

#' Compute the length of a line.
#' 
#' @param data containing the points of the line.
#' @return the length of the line.
path_lenght <- function(data){
  lenght <- 0
  for(row in 1:(dim(data)[1]-1)){
    lenght <- lenght + vector_distance(data[row,],data[row+1,]) 
  }
  lenght/dim(data)[1]
}

conditional_path_length <- function(data, conditional_function){
  lenght <- 0
  for(row in 1:(dim(data)[1]-1)){
    if (conditional_function(row)) lenght <- lenght + vector_distance(data[row,],data[row+1,]) 
  }
  lenght/dim(data)[1]
}

is_on_circuit <- function(input) any(input == 0)

step_on_circuit <- function(input, output){
  
  is_moving <- function(output){
    any(output == 1)
  }
  
  lenght <- 0
  for(row in 1:(dim(input)[1]-1)){
    if(is_moving(output[row,]) && is_on_circuit(input[row,])){
      lenght <- lenght + 1
    }
  }
  lenght/dim(input)[1]
}

max_distance <- function(data){
  max <- 0
  starting_position <- data[1,]
  for(row in 2:dim(data)[1]) {
    dist <- vector_distance(starting_position, data[row,])
    if(dist > max)
      max <- dist
  }
  max
}

max_distance2 <- function(positions,input){

  max <- 0
  starting_position <- positions[1,]
  for(row in 2:dim(positions)[1]) {
    if(is_on_circuit(input[row,])){
      dist <- vector_distance(starting_position, positions[row,])
      if(dist > max){
        max <- dist
      }
    }
  }
  max
}

avg_distance_from_light <- function(positions, light){
  distance <- 0
  length <- dim(positions)[1]
  for(row in 1:length) {
    distance <- distance + vector_distance(positions[row,], light)
  }
  1/(distance/length)
}

##################
### Statistics ###
##################

#' Generate all binary values from n bits.
#' 
#' @param n the number of bits.
#' @return the table of all combinations.
generate_all_binary_combinations <- function(n){
  z <- rep(0, n)
  do.call(rbind, lapply(0:n, function(i) t(apply(combn(1:n,i), 2, function(k) {z[k]=1;z}))))
}

generate_all_binary_combinations2 <- function(X,Y){
  m <- matrix(nrow = dim(X)[1] * dim(Y)[1], ncol = dim(X)[2] + dim(Y)[2])
  for(row_idX in 1:dim(X)[1]){
    for(row_idY in 1:dim(Y)[1]){
      m[(row_idX - 1) * dim(Y)[1] + row_idY,] <- c(X[row_idX,],Y[row_idY,])
    }
  }
  m
}

generate_all_binary_combinations3 <- function(X,Y,Z){
  m <- matrix(nrow = dim(X)[1] * dim(Y)[1] * dim(Z)[1], ncol = dim(X)[2] + dim(Y)[2] + dim(Z)[2])
  for(row_idX in 1:dim(X)[1]){
    for(row_idY in 1:dim(Y)[1]){
      for(row_idZ in 1:dim(Z)[1]){
        m[(row_idX-1)*(dim(Y)[1]*dim(Z)[1])+(row_idY-1)*dim(Z)[1]+row_idZ,] <- c(X[row_idX,],Y[row_idY,],Z[row_idZ,])
      }
    }
  }
  m
}

#' Computing occurrences of combinations in data.
#' 
#' @param combinations the table of all combinations.
#' @param data the to analyse.
#' @return the occurrences.
get_occurences_combinations <- function(combinations, data) {
  
  fast_unique_match <- function(x,X){
    for(i in 1:dim(X)[1]){
      if(all(x == X[i,])) break;
    }
    i
  }
  
  occurrences <- rep(0, dim(combinations)[1])
  for (id_row in 1:dim(data)[1]) {
    index <- fast_unique_match(data[id_row,],combinations)
    occurrences[index] <- occurrences[index] + 1
  }
  occurrences
}

##################
#### Measures ####
##################

# Entropy
fast_entropy <- function(X){
  e <- 0
  combX <- generate_all_binary_combinations(dim(X)[2])
  combX_occ <- get_occurences_combinations(combX,X)
  
  # Filtering non zero combinations
  combX <- combX[unlist(lapply(combX_occ, function(x) x > 0)),]
  combX_occ <- combX_occ[unlist(lapply(combX_occ, function(x) x > 0))]
  
  if(is.null(dim(combX))) combX <- matrix(combX,nrow=1,ncol=length(combX))
  
  if(length(combX) > 0){
    D <- dim(X)[1]
    S <- length(combX_occ)
    for(s in 1:S){
      e <- e + ((combX_occ[s]/D) * my_log2(combX_occ[s]/D))
    }
  }
  -e
}

# Predictive Information
fast_predictive_information <- function(X,Y){
  pi <- 0
  X <- X[1:dim(X)[1]-1,,drop=FALSE]
  Y <- Y[2:dim(Y)[1],,drop=FALSE]
  combX <- generate_all_binary_combinations(dim(X)[2])
  combY <- generate_all_binary_combinations(dim(Y)[2])
  combX_occ <- get_occurences_combinations(combX,X)
  combY_occ <- get_occurences_combinations(combY,Y)
  
  # Filtering non zero combinations
  combX <- combX[unlist(lapply(combX_occ, function(x) x > 0)),,drop=FALSE]
  combY <- combY[unlist(lapply(combY_occ, function(x) x > 0)),,drop=FALSE]
  combX_occ <- combX_occ[unlist(lapply(combX_occ, function(x) x > 0))]
  combY_occ <- combY_occ[unlist(lapply(combY_occ, function(x) x > 0))]
  
  if(is.null(dim(combX))) combX <- matrix(combX,nrow=1,ncol=length(combX))
  if(is.null(dim(combY))) combY <- matrix(combY,nrow=1,ncol=length(combY))
  
  combXY <- generate_all_binary_combinations2(combX,combY)
  combXY_occ <- get_occurences_combinations(combXY, cbind(X,Y))
  
  if(length(combXY) > 0){
    D <- dim(X)[1]
    S <- length(combX_occ)
    R <- length(combY_occ)
    for(s in 1:S){
      for(r in 1:R){
        pi <- pi + ((combXY_occ[(s-1)*R+r]/D) * my_log2((D*combXY_occ[(s-1)*R+r])/(combX_occ[s] * combY_occ[r])))
      }
    }
  }
  pi
}

# Transfer Entropy
fast_transfer_entropy <- function(X,Y){
  te <- 0
  tmp <- Y
  X <- X[1:dim(X)[1]-1,,drop=FALSE]
  Y <- Y[2:dim(Y)[1],,drop=FALSE]
  Z <- tmp[1:dim(tmp)[1]-1,,drop=FALSE]
  combX <- generate_all_binary_combinations(dim(X)[2])
  combY <- generate_all_binary_combinations(dim(Y)[2])
  combZ <- combY
  combX_occ <- get_occurences_combinations(combX,X)
  combY_occ <- get_occurences_combinations(combY,Y)
  combZ_occ <- get_occurences_combinations(combZ,Z)
  
  # Filtering non zero combinations
  combX <- combX[unlist(lapply(combX_occ, function(x) x > 0)),,drop=FALSE]
  combY <- combY[unlist(lapply(combY_occ, function(x) x > 0)),,drop=FALSE]
  combZ <- combZ[unlist(lapply(combZ_occ, function(x) x > 0)),,drop=FALSE]
  combX_occ <- combX_occ[unlist(lapply(combX_occ, function(x) x > 0))]
  combY_occ <- combY_occ[unlist(lapply(combY_occ, function(x) x > 0))]
  combZ_occ <- combZ_occ[unlist(lapply(combZ_occ, function(x) x > 0))]
  
  if(is.null(dim(combX))) combX <- matrix(combX,nrow=1,ncol=length(combX))
  if(is.null(dim(combY))) combY <- matrix(combY,nrow=1,ncol=length(combY))
  if(is.null(dim(combZ))) combZ <- matrix(combZ,nrow=1,ncol=length(combZ))
  
  combXYZ <- generate_all_binary_combinations3(combX,combY,combZ)
  combXYZ_occ <- get_occurences_combinations(combXYZ, cbind(X,Y,Z))
  
  combXZ <- generate_all_binary_combinations2(combX,combZ)
  combXZ_occ <- get_occurences_combinations(combXZ, cbind(X,Z))
  
  combYZ <- generate_all_binary_combinations2(combY,combZ)
  combYZ_occ <- get_occurences_combinations(combYZ, cbind(Y,Z))
  
  if(length(combXYZ) > 0){
    D <- dim(X)[1]
    S <- length(combX_occ)
    R <- length(combY_occ)
    Q <- length(combZ_occ)
    for(s in 1:S){
      for(r in 1:R){
        for(q in 1:Q){
          te <- te + ((combXYZ_occ[(s-1)*(R*Q)+(r-1)*Q+q]/D) *
                        my_log2(my_div(combXYZ_occ[(s-1)*(R*Q)+(r-1)*Q+q]*combZ_occ[q],
                                       combXZ_occ[(s-1)*Q+q] * combYZ_occ[(r-1)*Q+q])))
        }
      }
    }
  }
  te
}

