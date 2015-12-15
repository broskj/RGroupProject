install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)

#Change the path to the directory on your local machine where files are kept
setwd("/home/ashwani/Desktop/Random/Dataset/Group_Project")

#IMDB dataset
imdb_arm = read.csv("imdb_arm.csv", header = TRUE)

head(imdb_arm)

#Taking fields for people to check who work together often
data1 <- imdb_arm[,5:9]

head(data1)

#Mining association rules for the usual cast
betterRules <- apriori(data1, parameter = list(supp=0.001, conf=0.90))
inspect(betterRules)
plot(betterRules)

subset.matrix <- is.subset(betterRules,betterRules)
subset.matrix[lower.tri(subset.matrix,diag=T)] <- NA
redundant <- colSums(subset.matrix,na.rm=T) >= 1
uniqueBetterRules <- betterRules[!redundant]

inspect(uniqueBetterRules)
plot(uniqueBetterRules)
# Sorting based on lift
sortedUniqueBetterRules <- sort(uniqueBetterRules, by = "lift")
inspect(sortedUniqueBetterRules)
plot(sortedUniqueBetterRules)

#Relation beween Genre and cast
betterRules <- apriori(imdb_arm[,4:9], parameter = list(supp=0.001, conf=0.40))
genreRules <- subset( betterRules, subset = rhs %pin% "Genre=" )
genreRules
inspect( genreRules )
plot(genreRules)

subset.matrix <- is.subset(genreRules,genreRules)
subset.matrix[lower.tri(subset.matrix,diag=T)] <- NA
redundant <- colSums(subset.matrix,na.rm=T) >= 1
uniqueBetterRules <- genreRules[!redundant]

inspect(uniqueBetterRules)
plot(uniqueBetterRules)
# Sorting based on lift
sortedUniqueBetterRules <- sort(uniqueBetterRules, by = "confidence")
inspect(sortedUniqueBetterRules)
plot(sortedUniqueBetterRules)

#Detaching packages
detach("package:arulesViz", unload = TRUE)
detach("package:arules", unload = TRUE)
