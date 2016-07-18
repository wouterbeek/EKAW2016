library('igraph') #load igraph library
g <- read.graph("skoshastopcon.graphml",format=c("graphml") #read graphml file
mean(degree(g,mode = c("all") #average degree
mean(degree(g,mode =c("in") #in or out degree
transitivity(g,type = "global") #Global clustering coefficient
transitivity(g,type = "localaverage") #Local clustering coefficient
c <- components(g, mode ="weak") #number of connected components
bipartite_mapping(g) #Bipartite test
fit1 <- fit_power_law(d+1,10) #calculates power law fit
fit1 #shows p-value
fit2 <- fit_power_fit(d+1,10,implementation = "R.mle") #power law fit approximation to exponent 3. Suggested by R documentation
stats4::coef(fit2) #Corresponding alpha
library("ineq") #Loads inequality libraries
d<-degree(g,mode = c("all") #Average degree
ineq(d,type = "Gini") #Inequality coeff. of degrees
ineq(c$csize,type = "Gini") #Inequality coeff. of component sizes
