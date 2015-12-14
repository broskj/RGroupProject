#-- G16 -#
# Kyle Brost
#
#
#
# CIS4930
# December 17, 2015

# Classification (Apriori)- Genre Prediction
# Association Rule Mining - 'The Usual Casts'
# Clustering - Finding similar Movies

#####################################Edited##############################################################
install.packages("e1071")
install.packages("caret")
install.packages("gmodels")
library(e1071
library(caret)
library(gmodels)
imdb <- read.csv("imdb.csv", stringsAsFactors = TRUE) 
#naive bayes

#Recognize diffent genres of movies
set.seed(1071)

#Creating the training and test datasets
ind <- sample(2, nrow(imdb), replace=TRUE, prob=c(0.8, 0.2))
#Create training labels of genre
training.labels <- imdb[ind==1,3]
#Create test labels of genre
test.labels <- imdb[ind==2,3]
#Create training set
training <- imdb[ind==1,]
#Create test set
test <- imdb[ind==2,]

A_classifier <- naiveBayes(training, training.labels)
pred <- predict(A_classifier, test)
x =  table(pred, test.labels)
#Confusion Martix 
print(confusionMatrix(x))
CrossTable(pred, test.labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))
#####################################Edited##############################################################

# install packages
if(require('RWeka')) {
	library(RWeka)
} else {
	install.packages("RWeka")
	library(RWeka)
} if(require('partykit')) {
	library(partykit)
} else {
	install.packages("partykit")
	library(partykit)
}
if(require('arules')) {
	library(arules)
} else {
	install.packages('arules')
	library(arules)
}

# import dataset
ds <- read.csv(file='imdb.csv', header=F, sep=',')

# pause
pause <- function() {
	cat("\nPress <Enter> to continue")
	fil <- readLines(con="stdin", 1)
}

# create training and test sets
divideDataset <- function(dataframe, seed=NULL) {
	if(!is.null(seed)) set.seed(0156)
	index <- 1:nrow(dataframe)
	trainindex <- sample(index, trunc(length(index)*0.8))
	trainset <- dataframe[trainindex, ]
	testset <- dataframe[-trainindex, ]
	list(trainset = trainset, testset=testset)
}

# learn C4.5 fit
myC45 <- function(dataframe) {
	return(J48(genre~., data=dataframe))
}

# predict C4.5 fit
myC45Predict <- function(split, fit) {
	return(predict(fit, split$testset))
}

# C4.5: print final table and statistics
printfinal <- function(Prediction, Actual) {
	cat("Predicted matrix: \n")
	print(d)
	cat("\nPredicted:\t", predict)
	cat("\nTrue:\t\t", true)
	cat("\nPress <Enter> to continue")
    pause()
}

# apriori: convert to ruleset
myAssociationRuleMining <- function(myData, parameter = NULL, appearance = NULL) {
	if(is.null(parameter))
		return (apriori(myData, parameter=NULL, appearance)) # default ruleset
	else
		return (apriori(myData, parameter, appearance)) # better ruleset
}

# apriori: function to prompt whether to print larger rulesets
promptPrint <- function(ruleset, num) {
	cat("This rule set contains", num, "rules. Do you want to print the entire rule set? [y/n]\n")
	cat("Or press [h] to print the first few rules.\n")
	fil <- readLines(con="stdin", 1)
	if(fil == 'y' || fil == 'Y') {
		inspect(ruleset)
		pause()
	}
	else if(fil == 'h' || fil == 'H'){
		inspect(head(ruleset))
		pause()
	}
	else
		cat("Continuing.\n")
}

# apriori: sort better rules
mySorting <- function(rules) {
	return(sort(rules, decreasing=TRUE, na.last=NA, by="lift"))
}

# apriori: prune datasets
myRulePruning <- function(rules) {
	subset <- is.subset(rules, rules)
	subset[lower.tri(subset, diag=T)] <- NA
	redundant <- colSums(subset, na.rm=T) >= 1
	return(rules[!redundant])
}

cat("=====Classification=====\n")
# define randomized datasets
cat("Splitting dataset...")
ds$split <- divideDataset(ds, seed=0156)
# fit training sets
cat("done.\n\nFitting training set...")
pause()
ds$train <- myC45(ds$split$trainset)
# predict test sets
cat("done.\n\nPredicting test set...")
pause()
ds$predict <- myC45Predict(ds$split, ds$train)
# done
cat("done.\n\n=====Classification done=====\n")
pause()
# print
printfinal(ds$predict, ds$split$testset$genre)

cat("=====Association Rule Mining=====\n")
# mine basic rules
cat("Mining basic rules...")
#######################
# need to edit rules  #
# use this formatting #
#######################
ds$rules <- myAssociationRuleMining(ds[2:ncol(ds)], appearance = list(rhs=c("Survives=1", "Survives=0"), default="lhs"))
# mine better rules
cat("done.\n\nMining better rules...")
pause()
#######################
# need to edit rules  #
# use this formatting #
#######################
ds$betterrules <- myAssociationRuleMining(ds[2:ncol(ds)], parameter = list(supp=0.01, conf=0.90), appearance = list(rhs=c("Survives=1", "Survives=0"), default="lhs"))
# sort rules by lift
cat("done.\n\nSorting rules by lift...")
pause()
ds$sorted <- mySorting(ds$betterrules)
# prune sorted rules
cat("done.\n\nPruning sorted rules...")
pause()
ds$pruned <- myRulePruning(ds$sorted)
# done
cat("done.\n\n=====Association Rule Mining done=====\n")
pause()
# print
promptPrint(ds$pruned, length(ds$pruned))

cat("=====Clustering=====\n")
# k means clustering with k=32 (# of genres)
cat("\nk-means clustering...")
# perform PCA
ds$pc <- princomp(ds[1:3], cor=TRUE, scores=TRUE)
# cluster dataset with k=32
ds$cl <- kmeans(ds[1:3], 32)
ds$cluster <- as.factor(ds$cl$cluster)
cat("done.\n\n=====Clustering done=====\n")
pause()
plot(ds$cluster)
