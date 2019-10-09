library(bigmemory)
library(biganalytics)
library(bigstatsr)
library(FactoMineR)
library(biglasso)


#Loading the dataset ++ #Need to convert team into factor
x <- read.big.matrix("champ2.csv", sep = ",", type = "integer",
                     backingfile = "testo.bin", descriptor = "testo.desc",
                     shared = TRUE, header = TRUE)

champdesc <- dget("testo.desc")
champ <- attach.big.matrix(champdesc)

summary(champ)

champX <- as.big.matrix(champ[,-1])
champy <- as.numeric(champ[,1])

model = biglasso(champX, champy, family = "binomial")
summary(model)


data = read.csv("champignon.csv", sep = ",", header = TRUE)

data = data[,-1]
#Getting the explanatory variable
X <- as.matrix(data[,-1])

#The response variable
y <- as.numeric(data[,1])

#Checking the dim of the dataset
dim(X)

#Changing the type of the dataset X into a big matrix (big memory)
X.bm <- as.big.matrix(X)
str(X.bm) #It will be store on the hard drive instead of the RAM

#Fitting a Lasso regression on the big matrix X
fit = biglasso(X.bm, y, family = "binomial") #Binomial because our response variable y is a binary variable
#Plotting what we get
plot(fit)
