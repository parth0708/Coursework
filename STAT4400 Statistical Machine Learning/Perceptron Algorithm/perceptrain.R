perceptrain <- function(S,y){
  k = 1 #counter for iterations
  z1 = runif(ncol(S)) #initializing z
  z1_hist <- z1 #initializing z_hist
  cost = 1
  while(cost!= 0){
    gradcost = rep(0,ncol(S)) #initializing gradient cost
    f = classify(S,z1)
    for (i in 1:length(f)){
      if (f[i]!=y[i]){
        gradcost = gradcost - (y[i]*S[i,])
      }
    }
    z1_hist <- rbind(z1_hist,z1)
    z1 = z1 - (1/k)*gradcost #updating z
    k = k+1 #increment iterations
    cost = sum(gradcost) #summation of cost
  }
  return(list(z = z1,z_hist = z1_hist))
}