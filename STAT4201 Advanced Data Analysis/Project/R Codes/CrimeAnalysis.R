#Buckets: 10,15,20,30,40,60
library(leaps)

dat <- read.csv("Combined_dataset_tract level_v0.07_PP.csv", na.strings= "NA")
datnew <- matrix(unlist(dat),ncol = ncol(dat),byrow = F)
colnames(datnew) <- colnames(dat)
dat <- data.frame(na.omit(datnew))
dat <- dat[,-(1:3)] #removing ID, Tract, Boroughs
dat <- dat[,-(26:32)] #Removing individual felony data
dat <- dat[,-27] #Removing FelonyBucket
model <- lm(Total_Felony~.,data = dat)
summary(model)

#model1 <- glm(Total_Felony~.,data = dat, family = "binomial")
#dat <- cbind(dat[,1],dat[,14:25])
#model2 <- lm(dat$Total.population~.,data = dat[,14:25])

subset <- regsubsets(Total_Felony~.,data = train,nvmax = 20)
which.max(summary(subset)$adjr2)
#Best model: 17 var
ind <- c(3,11,13,17,19,20,21,24)

#splitting test and train
set.seed(1)
index <- sample(nrow(dat),0.75*nrow(dat),replace = FALSE)
train <- dat[index,]
test <- dat[-index,]
actual <- test$Total_Felony

#LM
model.lm <- lm(Total_Felony~.,data = train)
pred.lm <- predict(model.lm,test)
mse.lm <- mean((actual-pred.lm)^2) #638.4614

#Lasso
library(glmnet)
cv.out=cv.glmnet(as.matrix(train[,-26]),train$Total_Felony,alpha=1,lambda=c(0.01,0.1,1,10),nfolds=5)
bestlam=cv.out$lambda.min
model.lasso <- glmnet(as.matrix(train[,-26]),train$Total_Felony,alpha=1,lambda=bestlam)
pred.lasso <- predict(model.lasso,as.matrix(test[,-26]))
mse.lasso <- mean((actual-pred.lasso)^2) #637.8859

#Trees
library(tree)
model.trees <- tree(Total_Felony~., data = train[,-ind])
pred.tree <- predict(model.trees,test)
mse.tree <- mean((actual-pred.tree)^2) #644.567

#RF
library(randomForest)
model.rf <- randomForest(train[,-26],train[,26])
pred.rf <- predict(model.rf,test)
mse.rf <- mean((actual-pred.rf)^2) #539.0216
varImpPlot(model.rf)

#---------------------------------------------------------------------
library(maps)


#---------------------------------------------------------------------
indexrem <- c(8,10,11,13) #index of irrelavant variables
model <- lm(Total_Felony~.,data = dat[,-indexrem]) #adjR2 = ~0.6

#Checking RSS
datnew <- dat[,-indexrem]
index <- sample(nrow(dat),0.75*nrow(dat),replace = FALSE)
train <- datnew[index,]
test <- datnew[-index,]
modelsub <- lm(Total_Felony~.,data = train)
predsub <- 


#---------------------------------------------------------------------
library(ggplot2)
geo <- read.csv("Combined_dataset_tract level_v0.06_PP.csv", na.strings= "NA")$Id2
ggtract<-fortify(geo, region = "GEOID")

#library(rgdal)
#library(proj.4)

  ggplot() +
  geom_polygon(data = tract , aes(x=long, y=lat, group = group, fill=percent), color="grey50") +
  scale_fill_gradientn(colours = c("red", "white", "cadetblue"),
                       values = c(1,0.5, .3, .2, .1, 0))+
  coord_map(xlim = c(-74.26, -73.71), ylim = c(40.49,40.92))

tract <-readOGR(dsn = "/Users/parthpareek/Documents/Acads/02 Spring '16/03 ADA/Project/Raw/Archive/tl_2013_36_tract", layer = "tl_2013_36_tract")
tracts <- fortify(, region="GEO_ID")

# Setting PATH for Python 3.4
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
export PATH

# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH


