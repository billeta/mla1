#We'll assume that DataRugby is a huge dataset and load it as a big matrix
library(bigmemory)
library(biglm)
library(biganalytics)
library(bigstatsr)

#Loading the dataset ++ #Need to convert team into factor
x <- read.big.matrix("DataRugby.csv", sep = ";", type = "integer",
                     backingfile = "test.bin", descriptor = "test.desc",
                     shared = TRUE, 
                     col.names = c('Team', 
                                   'opposition', 
                                   'Result', 
                                   'nb_best_player', 
                                   'climb_rank',
                                   'world_ranking', 
                                   'penalties_2015', 
                                   'yellow_card_2015', 
                                   'runs_2015',
                                   'tries_score', 
                                   'conversions_score_2015', 
                                   'Opp_nb_best_player',
                                   'Opp_climb_rank', 
                                   'Opp_world_ranking', 
                                   'Opp_penalties_2015',
                                   'Opp_yellow_card_2015', 
                                   'Opp_runs_2015', 
                                   'Opp_tries_score',
                                   'Opp_conversions_score_2015'))


DataRugbydesc <- dget("test.desc")
DataRugby <- attach.big.matrix(DataRugbydesc)

DataRugby = DataRugby[-1,]


#We can start by a first overview by looking at the head of the dataset
head(DataRugby)
#We can check the summary to have an overview which are the variables
summary(DataRugby)

#This dataset contains observation on every rugby match that occurs this past 10 years 
#Each line is equal to a match between two team
###Explanation of every variables###
#Team: the team of the match (between 0 and 19, see the TeamIdx Data)
#opposition: The opposing team
#Results: 1 = Team wins and 0 = Team loses
#nb_best_player: number of best player in each team
#climb_rank: climb in the ranking ladder this past year
#world ranking: world rank on september 2019
#penalties, yellow card, runs, tries, conversions: stats on the past world cup

#We scale because all the value are not on the same scale
DataRugby[, 4:19] = scale(DataRugby[, 4:19])

#Load the factominer library to run a pca on our data
library(FactoMineR)

#performing PCA
pca <- PCA(DataRugby[,c(1,4:10)], quali.sup = 1)
plot.PCA(pca,choix="ind",axes = 1:2, label = "qual")

#Load the team index in order to which team is where
idx = read.csv("TeamIdx.csv", sep = ";")
colnames(idx) <- c("Country", "label")
idx

#We are not going to run a logistic regressions on the dataset to try to create a model that
#predict which team will win

#Setting train and test subset
smp_size <- floor(0.75 * nrow(DataRugby))

train.ind <- sample(seq_len(nrow(DataRugby)), size = smp_size)

train <- DataRugby[train.ind, ]
test <- DataRugby[-train.ind, ]

train <- as.big.matrix(train)
test <- as.big.matrix(test)

glm.model = glm(train[,3] ~ train[,4]
                                    + train[,5]
                                    + train[,6]
                                    + train[,7]
                                    + train[,8]
                                    + train[,9]
                                    + train[,10]
                                    + train[,11]
                                    + train[,12]
                                    + train[,13]
                                    + train[,14]
                                    + train[,15]
                                    + train[,16]
                                    + train[,17]
                                    + train[,18]
                                    + train[,19], family = binomial)
summary(glm.model)
predict(glm.model, as.data.frame(test))
X.bm <- as.big.matrix(X)





