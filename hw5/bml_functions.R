#################################################################################
#### Functions for BML Simulation Study

##Check out this website:
## www.jasondavies.com/bml

#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  total.spaces <- r*c
  m <- matrix(sample(c(0,1,2),total.spaces,replace=TRUE,prob=c(1-p,p/2,p/2)),r,c)
  return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.
test.mat <- matrix(c(1,0,0,2,1,1,0,0,0,1,2,0,0,2,1,1,1,2,0,0,0,2,0,0,0),5,5)

bml.step <- function(m){
  mstar <- m
  mstar <- mstar[,c(ncol(m),1:(ncol(m)-1))]
  add.new.reds <- 1*((m==0)&(mstar==1))
  subtract.old.reds <- add.new.reds[,c(2:ncol(m),1)]
  red.shift <- (m - subtract.old.reds) + add.new.reds
  blue.shift <- red.shift[c(2:nrow(red.shift),1),]
  add.new.blues <- 2*((red.shift==0)&(blue.shift==2))
  subtract.old.blues <- add.new.blues[c(nrow(red.shift),1:(nrow(red.shift)-1)),]
  new.m <- (red.shift - subtract.old.blues) + add.new.blues
  grid.new <- any(m!=new.m)
  return(list(new.m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m <- bml.init(r,c,p)
  i=1
  while(i<=1000){
    step.output <- bml.step(m)
    m <- step.output[[1]]
    grid.new <- step.output[[2]]
    if (grid.new == FALSE){
      summary <- list("Gridlocked Matrix" = m,"Total Timesteps" = i,"Traffic Density" = p)
      return(summary)
      break} 
    else {i <- i + 1}
  }
  return(list("Free Flowing Matrix" = m, "Total Timesteps" = i-1, "Traffic Density" = p))
}
