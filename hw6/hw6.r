# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

initial.doctors <- sample(0:1,size=30,replace=TRUE,prob=c(0.9,0.1))

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  has_adopted <- matrix(, nrow=n.doctors, ncol=n.days)
  for(i in 1:n.days){
    meeting.docs <- sample(1:n.doctors,size=2,replace=FALSE)
    docs.adopt <- c(initial.doctors[meeting.docs[1]],initial.doctors[meeting.docs[2]])
    if (docs.adopt[1] != docs.adopt[2]){
      persuasion <- sample(0:1,size=1,prob=c(1-p,p))
      initial.doctors <- replace(initial.doctors,meeting.docs[docs.adopt==0],persuasion)}
    has_adopted[,i] <- initial.doctors
    }
  return(has_adopted)
}

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

twoMat <- sim.doctors(initial.doctors,30,50,0.2)
fourMat <- sim.doctors(initial.doctors,30,50,0.4)
fiveMat <- sim.doctors(initial.doctors,30,50,0.5)
sevenMat <- sim.doctors(initial.doctors,30,50,0.7)
nineMat <- sim.doctors(initial.doctors,30,50,0.9)

plot(x=1:50,y=apply(twoMat,2,sum),type="l",
     ylim=range(0,20),
     main="Gradual Adopting of a Drug by Doctors",
     ylab="Number of Doctors",
     xlab="Days",
     col="Red")
lines(x=1:50,y=apply(fourMat,2,sum))
lines(x=1:50,y=apply(fiveMat,2,sum),col="Green")
lines(x=1:50,y=apply(sevenMat,2,sum),col="Orange")
lines(x=1:50,y=apply(nineMat,2,sum),col="Purple")
legend("topleft",c("0.2","0.4","0.5","0.7","0.9"),title="Conversion Probability",
       fill=c("red","black","green","orange","purple"))