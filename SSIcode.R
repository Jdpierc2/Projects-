library(GameTheory)


methods('ShapleyShubik')
getAnywhere('ShapleyShubik')

A single object matching 'ShapleyShubik' was found
It was found in the following places
package:GameTheory
namespace:GameTheory
with value


#The Guts of the ShapleyShubik function 
SSI2=function (quota, y, Names = NULL) 
{
  n <- length(y)
  res1 <- permutations(n, n)
  res2 <- apply(res1, 1, function(x) {
    x[sum(cumsum(y[x]) < quota) + 1]
  })
  
  
  
  
  ? permutations
  res2 <- as.numeric(res2)
  Power <- matrix(NA, ncol = 1, nrow = n)
  for (i in 1:n) {
    Power[i, 1] <- sum(res2 == i)
  }
  SHI <- Power/factorial(n)
  TABLE <- rbind(y, y/sum(y), t(SHI))
  rownames(TABLE) <- c("Votes", "Votes (%)", "Shapley-Shubik")
  colnames(TABLE) <- Names
  Output <- list(Results = TABLE, Distribution = y, C, Method = "PowerIndex", 
                 Quota = quota, Names = Names)
  class(Output) <- "ShapleyShubik"
  return(Output)
}
SSI2(quota, y, Names)
<environment: namespace:GameTheory>
  
  
#Trying to add a second quota to the SSI
quota= 7 #I've made a basic example to get the code right. 
quota2=3
y= c(4,3,2,1)
p= c(1,1,1,1)
Names= c('A', "B", "C", "D")

SSI = function (quota, quota2, y, p, Names ) 
{
  n <- length(y)
  
  res1 <- permutations(n, n) #SSI needs all of the possible ways to position the four players. 
  #SSI cares about the order of the players so finding all ways to order them is important
  res2 <- apply(res1, 1, function(x) {
    x[sum(cumsum(y[x]) < quota) + 1] #This is the basic quota for SSI: a quota of a certain amount 
    #of votes that are needed for the motion to pass
    x[sum(cumsum(p[x]) < quota2) + 1]# I am trying to add a second quota to measure power in the new
    #voting system in the EU. It now requries a certain number of players to be in the winning coalition 
    #for the vote to pass. 
    #Right now the function only captures quota 2; it does not pay attention to the times where 
    # quota 2 is reached (there are three players) but quota 1 is not reached (there isn't 7 votes)
  })
  
  
  res2 <- as.numeric(res2)
  
  #?as.numeric: converts the index part of the factor into numeric vector
  
  
  Power <- matrix(NA, ncol = 1, nrow = n)
  for (i in 1:n) {
    Power[i, 1] <- sum(res2 == i)
  } #Creates a matrix with one column where each row is the number of times a player is pivotal
  
  
  SHI <- Power/factorial(n) # This is the calculation of power based on the SSI formula. So it takes the 
  #Number of times a player is pivotal and divides it by the number of possible coalitions. 
  
  
  TABLE <- rbind(y, y/sum(y), t(SHI))
  rownames(TABLE) <- c("Votes", "Votes (%)", "Shapley-Shubik")
  colnames(TABLE) <- Names
  #Returns a table which is the power index of the game. Columns are the players. 1st row 
  # is the number of votes the player has. Second row seems to be the precentage of all of the votes 
  # the player has? Last row is their power. 
  
  Output <- list(Results = TABLE, Distribution = y, C, Method = "PowerIndex", 
                 Quota = quota, Quota2=quota2, Names = Names)
  class(Output) <- "ShapleyShubik"
  return(Output) 
  #Defines what the function puts out. 
}

SSI(quota, quota2, y, p, Names)


res2 <- apply(res1, 1, function(x) {
  x[sum(cumsum(p[x]) < quota2) + 1]
  if(any(x<=quota)){}

  