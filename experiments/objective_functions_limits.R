library("png")
source("statistic_functions.R")

# X and Y axis are switched
path <- readPNG("../src/picasso.PNG")
path <- path[,,1]

zeros <- length(path[unlist(lapply(path, function(x) x == 0))])
ones <- length(path[unlist(lapply(path, function(x) x == 1))])

print(concat(c("Circuit surface over arena surface is: ", zeros/length(path))))

furthest_point <- function(start, path){
  
  tmp_dist = 0
  result <- start
  for(x in 1:dim(path)[1]){
    for(y in 1:dim(path)[2]){
      if(path[y,x]==0){
        d <- vector_distance(start, c(y,x))
        if(d>tmp_dist){
          tmp_dist <- d
          result <- c(y,x)
        }
      }
    }
  }
  result
}

pf_starting_position <- c(112,149) #{-0.771,1.327} converted into pixels with axis switch
p <- furthest_point(pf_starting_position,path)
# Conversion from 4x4 to 6x6 and axis conversion
p <- c(-(200-p[2])/100*1.5,-(p[1]-200)/100*1.5)
# compute the line starting_position - p and then the intersection between the line and the circumference in p with radius 0.085.
# computed manually.
p <- c(2.4123, -2.1628)
pf_starting_position <- c(-0.771,1.327)

print("PATH FOLLOWING")
print(concat(c("Furthest point reachable on the path is ", p[1], ";", p[2])))
print(concat(c("Objective function's upper bound is ", vector_distance(pf_starting_position,p))))

light <- c(1.5,0,0.5)
pt_starting_position <- c(-1.857, 1.474)
closest_corner <- c(-3.865,3.865)
pt_time <- 500
velocity <- 0.015

move_towards_point <- function(position, end, velocity){
  if(vector_distance(position,end)>=velocity){
    ratio <- atan2((end[2]-position[2]),(end[1]-position[1]))
    c(position[1]+cos(ratio)*velocity,position[2]+sin(ratio)*velocity)
  } else position
}

dist <- 0
new_position <- pt_starting_position
for(t in 1:pt_time){
  dist <- dist + vector_distance(new_position,light)
  new_position <- move_towards_point(new_position,closest_corner,velocity)
}
print("PHOTO-TAXIS")
print(concat(c("Objective function's lower bound is ", pt_time/dist)))

dist <- 0
new_position <- pt_starting_position
for(t in 1:pt_time){
  dist <- dist + vector_distance(new_position,light)
  new_position <- move_towards_point(new_position,light,velocity)
}
print(concat(c("Objective function's upper bound is ", pt_time/dist)))

