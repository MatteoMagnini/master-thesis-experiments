# Test statistic_functions.R suit
# Run this script to ensure that Information Theory functions are correct.
# Firstly Entropy is deeply tested, then the other functions are tested with the help of Entropy.
# Predictive Information and especially Transfer Entropy are too complex to be tested as stand alone functions.

library(assertr)
library(dplyr)
source("statistic_functions.R")

#' Create a dataframe of one row with the metrics computed on the sensors and actuators' history.
#' 
#' @param input sensors' history.
#' @param output actuators' history.
#' @return a single row dataframe with the metrics.
get_measures <- function(input,output){
  se <- fast_entropy(input)
  pi <- fast_predictive_information(input,output)
  te <- fast_transfer_entropy(input,output)
  rte <- fast_transfer_entropy(output,input)
  data.frame(se,pi,te,rte)
}

#' Compare two values with a precision threshold.
#' 
#' @param x first value.
#' @param y second value.
#' @param precision the precision threshold.
#' @return TRUE if values are the same, FALSE otherwise.
equals <- function(x,y,precision=1e-6) abs(x-y) < precision

#' Computes the Transfer Entropy in function of Entropies.
#' 
#' @param X the first variable's history.
#' @param Y the second variable's history.
#' @return the Transfer Entropy.
transfer_entropy_by_entropies <- function(X,Y){
  te <- 0
  tmp <- Y
  X <- X[1:dim(X)[1]-1,,drop=FALSE]
  Y <- Y[2:dim(Y)[1],,drop=FALSE]
  Z <- tmp[1:dim(tmp)[1]-1,,drop=FALSE]
  
  - fast_entropy(cbind(X,Y,Z)) + fast_entropy(cbind(Y,Z)) + fast_entropy(cbind(X,Z)) - fast_entropy(Z)
}

#' Computes the Predictive Information in function of Entropies.
#' 
#' @param X the first variable's history.
#' @param Y the second variable's history.
#' @return the Predictive Information.
predictive_information_by_entropies <- function(X,Y){
  te <- 0
  tmp <- Y
  X <- X[1:dim(X)[1]-1,,drop=FALSE]
  Y <- Y[2:dim(Y)[1],,drop=FALSE]
  
  fast_entropy(X) + fast_entropy(Y) - fast_entropy(cbind(X,Y))
}

#' Test if metrics computed with the original formula are equals to metrics computed with entropies.
test_metrics_with_entropies <- function(experiment){
  experiment %>%
    verify(has_all_names("i","o"))
  
  input <- as.data.frame(experiment$i)
  output <- as.data.frame(experiment$o)
  
  input %>%
    assert(in_set(0,1), everything())
  
  output %>%
    assert(in_set(0,1), everything())
  
  measures <- get_measures(input,output)
  
  measures %>%
    verify(equals(pi,predictive_information_by_entropies(input,output))) %>%
    verify(equals(te,transfer_entropy_by_entropies(input,output))) %>%
    verify(equals(rte,transfer_entropy_by_entropies(output,input)))
}


##### TESTS #####

### ENTROPY ###

## Test that the entropy of a certain event is zero ##

# 10 tails.
stopifnot(equals(fast_entropy(data.frame(rep(1,10))),0))

# 20 heads.
stopifnot(equals(fast_entropy(data.frame(rep(0,20))),0))


## Test that the entropy of two certain dependent events is zero ##

# 10 heads with the first coin and 10 heads with the second coin.
stopifnot(equals(fast_entropy(cbind(data.frame(rep(0,10)),data.frame(rep(0,10)))),0))

# 10 heads with the first coin and 10 tails with the second coin.
stopifnot(equals(fast_entropy(cbind(data.frame(rep(0,10)),data.frame(rep(1,10)))),0))


## Test that the entropy of a fair random event is the base two logarithm of all possible different results ##

# 5 heads and 5 tails.
stopifnot(equals(fast_entropy(data.frame(c(rep(0,5),rep(1,5)))),1))

# 10 tails and 10 heads.
stopifnot(equals(fast_entropy(data.frame(c(rep(1,10),rep(0,10)))),1))


## Test that the entropy of two independent fair random event is the base two logarithm of all possible different results ##

# head for both coins 5 times, head and tail for 5 times, tail and head for 5 times, tails for both coins for 5 times.
stopifnot(equals(fast_entropy(cbind(data.frame(c(rep(0,10),rep(1,10))),data.frame(c(rep(0,5),rep(1,5),rep(0,5),rep(1,5))))),2))


## Test that a partially random event has entropy between 0 and the base two log of the possible results ##

# A coin that has three time heads has entropy equals to -0.75*log(0.75,2)-0.25*log(0.25,2) = 0.8112781.
stopifnot(equals(fast_entropy(data.frame(c(rep(0,15),rep(1,5)))), 0.8112781))

# A coin that has four time tails has entropy equals to -0.2*log(0.2,2)-0.8*log(0.8,2) = 0.7219281.
stopifnot(equals(fast_entropy(data.frame(c(rep(1,10),rep(0,40)))), 0.7219281))

## Test that the entropy of two independent event, one certain and the other fully random is half the base two logarithm of all possible different results ##

# Both heads for 5 times, head and tail for five times.
stopifnot(equals(fast_entropy(cbind(data.frame(rep(0,10)),data.frame(c(rep(0,5),rep(1,5))))),1))


### OTHER METRICS ###

# With sensors and actuators always equal to one all measures are zero.
test_metrics_with_entropies(setNames(data.frame(rep(0,10),rep(0,10)), c('i','o')))

# With fair random sensors and actuators always equal to one all measures are zero except sensor entropy which is one.
test_metrics_with_entropies(setNames(data.frame(c(rep(0,10),rep(1,10)),rep(0,20)), c('i','o')))

# With random sensors and actuators all measures are grater than zero.
test_metrics_with_entropies(setNames(data.frame(sample(c(0,1), replace=TRUE, size=20),sample(c(0,1), replace=TRUE, size=20)), c('i','o')))