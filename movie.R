# =====================================================================
# For compatibility with Rscript.exe: 
# =====================================================================

if(length(.libPaths()) == 1){
  # We're in Rscript.exe
  possible_lib_paths <- file.path(Sys.getenv(c('USERPROFILE','R_USER')),
                                  "R","win-library",
                                  paste(R.version$major,
                                        substr(R.version$minor,1,1),
                                        sep='.'))
  indx <- which(file.exists(possible_lib_paths))
  if(length(indx)){
    .libPaths(possible_lib_paths[indx[1]])
  }
  # CLEAN UP
  rm(indx,possible_lib_paths)
}


#============================
setwd("/Users/xucc/Documents/GMU/STAT515/Assignment/team project")
mydata_act5 <- read.csv("movie_actor_5.csv")
head(mydata_act5)

rows <- nrow(mydata)
colnames(mydata_act5)<- c("DireName","QuaDire","NumReview","DireFbLike","Act1FbLike","Gross","Act1Name","QuaAct1","NumVoted",
                          "CastFbLike","NumUserRew","Budget","Year","IMDBScore","MovFbLike","QuaMovie")
head(mydata_act5)
mydata_act5 <- na.exclude(mydata_act5)
rows <- nrow(mydata_act5)


library(tree)

set.seed(12345)
trainindex <- sample(rows, 0.6*rows, replace = FALSE)
training_act5 <- mydata_act5[trainindex,]
validation_act5 <- mydata_act5[-trainindex,]
head(training_act5)

#############
tree.act5 = tree(QuaMovie~.-(DireName+Act1Name+IMDBScore),training_act5)
summary(tree.act5)
plot(tree.act5)
text(tree.act5)
tree.act5

test.act5 = predict(tree.act5,validation_act5,type = "class")
table(test.act5, validation_act5$QuaMovie)
(145+371)/(0.4*rows)


#===========cross validation for tree.act5
set.seed(1)
cv.movies = cv.tree(tree.act5,FUN=prune.misclass)
names(cv.movies)
cv.movies

par(mfrow=c(1,2))
plot(cv.movies$size,cv.movies$dev,type="b")
plot(cv.movies$k,cv.movies$dev,type="b")

par(mfrow=c(1,1))

prune.movies = prune.misclass(tree.act5,best=5)
plot(prune.movies)
text(prune.movies,pretty=0)

testCv.act5 = predict(prune.movies,validation_act5,type="class")
table(testCv.act5, validation_act5$QuaMovie)
(156+376)/(0.4*rows)


library(rpart)				        # Popular decision tree algorithm
library(rattle)				      	# Fancy tree plot
library(rpart.plot)			    	# Enhanced tree plots
library(RColorBrewer)			  	# Color selection for fancy tree plot
library(party)					      # Alternative decision tree algorithm
library(partykit)				      # Convert rpart object to BinaryTree
library(caret)	

#myvals = c(2:10,12,13)
#mydata = mydata[myvals]
#tree1.noact3 = rpart(QuaMovie~.,training)



myvals = c(2:6,8:13,15,16)
mydata = mydata_act5[myvals]
tree1.act5 = rpart(QuaMovie~.,mydata)

plot(tree1.act5)
text(tree1.act5)
rpart.plot(tree1.act5) 
prp(tree1.act5, faclen = 0, cex = 0.8, extra = 1)


#======================random forest (no missing data allowed in RF)
library(randomForest)

training1_act5 = mydata[trainindex,]
validation1_act5 = mydata[-trainindex,]
head(training1_act5)
randomTree <- randomForest(QuaMovie~.,training1_act5)
#randomTree <- randomForest(QuaMovie~.-(DireName+Act1Name+IMDBScore),training_act5)
radtree.pred = predict(randomTree,validation1_act5,type = "class")
table(radtree.pred,validation1_act5$QuaMovie)
(150+380)/(0.4*rows)


#=====================knn
library(class)
t<-20
trainKnn <- sample(1:rows,t)

#convert categorial varibalbes to numerical, because knn can no deal categorial variable
attach(mydata_act5)
mydata_act5$QuaDireNew[QuaDire == "Excellent"] <- 5
mydata_act5$QuaDireNew[QuaDire == "Very Good"] <- 4
mydata_act5$QuaDireNew[QuaDire == "Good"] <- 3
mydata_act5$QuaDireNew[QuaDire == "Fair"] <- 2
mydata_act5$QuaDireNew[QuaDire == "Poor"] <- 1
mydata_act5$QuaAct1New[QuaAct1 == "Poor"] <- 1
mydata_act5$QuaAct1New[QuaAct1 == "Fair"] <- 2
mydata_act5$QuaAct1New[QuaAct1 == "Good"] <- 3
mydata_act5$QuaAct1New[QuaAct1 == "Very Good"] <- 4
mydata_act5$QuaAct1New[QuaAct1 == "Excellent"] <- 5
detach(mydata_act5)
head(mydata_act5)



#select features
myvals <- c(3:6,9:13,15,17,18)
x<- mydata_act5[myvals]
nearest1 <- knn(train = x[trainKnn,],test = x[-trainKnn,], cl=mydata_act5[trainKnn,16],k=1)
nearest5 <- knn(train = x[trainKnn,],test = x[-trainKnn,], cl=mydata_act5[trainKnn,16],k=5)

pcorn1 <- 100*sum(mydata_act5[-trainIndex,16]==nearest1)/(rows-t)
pcorn5 <- 100*sum(mydata_act5[-trainIndex,16]==nearest5)/(rows-t)

pcorr <- array(dim=c(1,10))

for (k in 1:10) {
  pred <- knn.cv(x,mydata_act5[,16],k)
  pcorr[1,k] <- round(100*sum(mydata_act5[,16]==pred)/rows,2)
  }

pcorr

nearest9 <- knn(train = x[trainKnn,],test = x[-trainKnn,], cl=mydata_act5[trainIndex,16],k=9)
valid.quamoive <- mydata_act5[-trainKnn,16]
table(nearest9,valid.quamoive)
(3+1030)/(1+4+418+1032)

#============new training data and validation data
trainingNew_act5 <- mydata_act5[trainindex,]
validationNew_act5 <- mydata_act5[-trainindex,]
head(trainingNew_act5)

#try numerical variable instead of nomial viriables of QualDirect, QualActor
treenew.act5 = tree(QuaMovie~.-(DireName+QuaDire+Act1Name+QuaAct1+IMDBScore),trainingNew_act5)
summary(treenew.act5)
plot(treenew.act5)
text(treenew.act5)

testnew.act5 = predict(treenew.act5,validationNew_act5,type = "class")
table(testnew.act5, validation_act5$QuaMovie)
(146+372)/(0.4*rows)


#============
randomTreeNew <- randomForest(QuaMovie~.-(DireName+QuaDire+Act1Name+QuaAct1+IMDBScore),trainingNew_act5)
radtreeNew.pred = predict(randomTreeNew,validationNew_act5,type = "class")
table(radtreeNew.pred,validationNew_act5$QuaMovie)
(152+381)/(0.4*rows)


