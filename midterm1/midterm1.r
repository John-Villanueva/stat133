# Please load in the dataset included in the midterm1 directory. It will be
# required to perform the following tasks. The dataset includes data for countries in 2012.

# calculate the mean and the maximum of pop (population) in the dataset. Store these as the
# variables <mean.pop> and <max.pop> respectively.

mean.pop <- mean(SO2012Ctry$pop)
max.pop <- max(SO2012Ctry$pop)

# For each country in the dataset, calculate the number of female athletes (Female) divided
# by the total number of athletes (Female + Male). Store this as the variable
# <female.prop>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset.

female.prop <- SO2012Ctry$Female/(SO2012Ctry$Female+SO2012Ctry$Male)

# Create the following two subsets of the dataset and store them as variables with the
# indicated names:
# 1) Countries with 0 silver medals: <subset.nosilver>
subset.nosilver <- SO2012Ctry[SO2012Ctry$Silver==0,]
# 2) Countries with more than or exactly 3 silver medals: <subset.threesilver>
subset.threesilver <- 

subset.nosilver <- SO2012Ctry[SO2012Ctry$Silver==0,]
subset.threesilver <- SO2012Ctry[SO2012Ctry$Silver>=3,]

# For each of your subsets, create a vector giving the population size. Store
# these as variables <subset.nosilver.pop> and <subset.threesilver.pop>.

subset.nosilver.pop <- subset.nosilver$pop
subset.threesilver.pop <- subset.threesilver$pop


# Implement the function meanpopByGDPPP. Your function should take the following
# arguments:
#
# <GDPPP.cutoff>: a numeric constant giving a cutoff to subset by
# <GDPPP>: a numeric vector of GDP per person
# <pop>: a numeric vector of populations
#   (this should be the same length as <GDPPP>)
#
# Your function should return the mean of the populations of countries
# whose values in <GDPPP> are strictly less that <GDPPP.cutoff>.

meanpopByGDPPP <- function(GDPPP.cutoff, GDPPP, pop){
 meanPOP <- mean(pop[GDPPP<=GDPPP.cutoff])
 return(meanPOP)
}

# Please create a plot of the proportion of female athletes (y-axis) 
# against the total number of athletes (x-axis). Your plot should include the following 
# features:
# 1) a title "Proportion of female athletes vs Total # athletes"
# 2) axis labels: "Proportion of female athletes" and "Total # athletes"
# 3) plotting character set to 19
# 4) a green horizontal line at female proportion of 0.50.
install.packages("ggplot2")
library("ggplot2")

install.packages("plyr")
library("plyr")

SO2012Ctry <- data.frame(SO2012Ctry,female.prop)
TotalAthletes <- SO2012Ctry$Female + SO2012Ctry$Male
SO2012Ctry <- data.frame(SO2012Ctry,TotalAthletes)

plot <- ggplot(data=SO2012Ctry, aes(x=TotalAthletes,y=female.prop)) +
  geom_point(fill=NA) +
  labs(list(title="Proportion of female athletes vs Total # athletes",x="Total # athletes",y="Proportion of female athletes")) +
  geom_hline(yintercept=0.5,color="green")


