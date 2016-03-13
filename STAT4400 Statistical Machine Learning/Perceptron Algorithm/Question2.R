source("fakedata.R")
#source("classify.R")
#source("perceptrain.R")
#source("plotplane.R")

z <- runif(3)
n <- 100
data <- fakedata(z,n)
S <- data$S
y <- data$y

#Part A
classify <- function(S, z){
  y <- vector(mode = "numeric",length=0)
  for (i in 1:nrow(S)){
    x1 <- c(S[i,])
    y[i] <- sign(sum(x1*z))
  }
return(y)
}

#Part B
perceptrain <- function(S,y){
  k = 1 #counter for iterations
  z1 = runif(ncol(S)) #initializing z
  z1_hist <- z1 #initializing z_hist
  cost = 1
  plot(S[which(y == 1),1],
       S[which(y == 1),2],
       col = "blue", xlab = "x", ylab = "y", 
       xlim = range(-5:5),ylim = range(-5:5))
  points(S[which(y == -1),1],
         S[which(y == -1),2],
         col = "green")
  while(cost!= 0){
    gradcost = rep(0,ncol(S)) #initializing gradient cost
    f = classify(S,z1)
    plotplane(z1,"black")
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
  z1_hist <- z1_hist[2:nrow(z1_hist),]
  rownames(z1_hist) <- NULL
  plotplane(z1,"red")
  return(list(z = z1,z_hist = z1_hist))
}

#Part C
set.seed(45)
z <- runif(3)
trainset <- fakedata(z,100)
ztrain <- perceptrain(trainset$S,trainset$y)$z
zhist <- perceptrain(trainset$S,trainset$y)$z_hist
testset <- fakedata(z,100)
ytest <- testset$y
ftest <- classify(testset$S,ztrain)
table(ftest,ytest)

#Part D
plot(testset$S[which(testset$y == 1),1],
     testset$S[which(testset$y == 1),2],
     col = "blue", xlab = "x", ylab = "y", 
     xlim = range(-5:5),ylim = range(-5:5))
points(testset$S[which(testset$y == -1),1],
       testset$S[which(testset$y == -1),2],
       col = "green")
plotplane(z,"black")
plotplane(ztrain,"red")

plotplane <- function(z, color){
  x <- z[1]
  y <- z[2]
  c1 <- z[3]
  m <- -x/y
  vhmod <- 1/sqrt(x^2 + y^2)
  c <- -sign(y)*c1*vhmod*sqrt(1 + m^2)
  abline(a = c, b = m, col = color)
}

