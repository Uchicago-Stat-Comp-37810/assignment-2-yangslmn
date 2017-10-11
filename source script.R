likelihood <- function(param){ # param is a vector with 3 entries
  a = param[1]
  b = param[2]
  sd = param[3]
  
  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T) # normal distribution of each value y
  sumll = sum(singlelikelihoods) # because they are log value, add them. 
  return(sumll)   # return the liklehood
}


slopevalues <- function(x){return(likelihood(c(x, trueB, trueSd)))}

prior <- function(param){
  a = param[1]
  b = param[2]
  sd = param[3]
  aprior = dunif(a, min=0, max=10, log = T) #assume that prior distribution of a is uniform.
  bprior = dnorm(b, sd = 5, log = T) # assume normal distribution
  sdprior = dunif(sd, min=0, max=30, log = T)  # assume unifrom.
  return(aprior+bprior+sdprior)
}

posterior <- function(param){
  return (likelihood(param) + prior(param)) #because it is log, add the results. 
}


proposalfunction <- function(param){ # random values from normal distribution
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

run_metropolis_MCMC <- function(startvalue, iterations){
  chain = array(dim = c(iterations+1,3)) # make the chain vector as matrix with indicated dim. 
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,]) # from the 1st row to the ith row, apply proposal function.
    
    probab = exp(posterior(proposal) - posterior(chain[i,])) # because it is log, this formula is same with the ratio. 
    if (runif(1) < probab){ #random value in uniform [0,1]
      chain[i+1,] = proposal  # accept when the ratio is bigger than p.
    }else{
      chain[i+1,] = chain[i,]  # reject when the ratio is not bigger than p
    }
  }
  return(chain)
}

graph_function<- function(chain, burnIn)
{ par(mfrow = c(2,3))
  alphabet<-c('a','b','sd')
  true<-c(trueA, trueB, trueSd)
  for(i in 1:3)
  {
    hist(chain[-(1:burnIn), i], nclass=30, main=paste("Posterior of",alphabet[i]),xlab="True value = red line")
    abline(v=mean(chain[-(1:burnIn), i]))
    abline(v= true[i], col='red')}
  for(j in 1:3)
  { plot(chain[-(1: burnIn), j], type = 'l', xlab = 'True value=red line',main = paste('Chain value of',alphabet[j]))
    abline(h=true[j], col='red')}
  
}
