setwd("D:/course - winter 2023/Fundamental Analytic/fifa_2022_datasets")
library(readxl)
fifa.df <- read_excel("Career Mode player datasets - FIFA 15-22.xlsx")
head(fifa.df)
str(fifa.df)
fifa.df <- fifa.df[-c(1:5)]
str(fifa.df)
ncol(fifa.df)
fifa.df <- fifa.df[-c(6,10,11,13)]
ncol(fifa.df)
str(fifa.df)
fifa.df <- fifa.df[-c(11,12,15,17)]
ncol(fifa.df)
fifa.df <- subset(fifa.df, select = -c(work_rate,body_type, release_clause_eur, player_tags,player_traits ))
fifa.na <- sapply(fifa.df, function(x) sum(is.na(x)))
fifa.df <- subset(fifa.df, select = -c(player_face_url,club_logo_url,nation_logo_url, nation_flag_url))
fifa.na <- sapply(fifa.df, function(x) sum(is.na(x)))
fifa.df <- subset(fifa.df, select = -c(club_flag_url,mentality_composure))
fifa.df <- subset(fifa.df, select = -c(nation_jersey_number,nation_jersey_number,nation_team_id ))
fifa.df <- subset(fifa.df, select = -c(goalkeeping_speed ))

#Count na value
sapply(fifa.df, function(x) sum(is.na(x)))
order(fifa.na, na.last = TRUE, decreasing = TRUE )
colnames(fifa.df[,c(51,13,14,60,20,21,22,23,24,25)])

# Count na value of a column
sum(is.na(fifa.df$shooting))

# Drop row when column has NA value
#Method 1
library(tidyr)
fifa.df = fifa.df %>% drop_na(shooting)

#Method 2
fifa.df = fifa.df[!is.na(fifa.df$club_team_id),]

# Method 3
fifa.df[complete.cases(fifa.df$wage_eur),]

# Method 4
fifa.df = subset(fifa.df, !is.na(value_eur))

# Check again NA
sum(is.na(fifa.df))

ncol(fifa.df)
str(fifa.df)
head(fifa.df)


#drop all columns between ls and gk
library(dplyr)
fifa.df <- fifa.df %>% select (-c(ls:gk))

#Solve categorical data
table(fifa.df$preferred_foot)
table(fifa.df$real_face)
fifa.df$preferred_foot <- as.factor(fifa.df$preferred_foot) #Label categorical data with num
fifa.df$real_face <- as.factor(fifa.df$real_face)

# Devide feature and target
fifa.df <- subset(fifa.df, select = -c(value_eur))
feature <- subset(fifa.df, select = -c(wage_eur))


#Recursive Feature Selection
library(caret)
filterCtrl2 <- rfeControl(functions=rfFuncs, method="cv", number=3)
results <- rfe(x= feature,y= fifa.df$wage_eur, sizes=c(1:9), rfeControl=filterCtrl2)
results
plot(results, type=c("g", "o"))

#convert categorical to numeric for numeric methods
fifa.df2 = fifa.df
fifa.df2[, c('preferred_foot', 'real_face')] <- sapply(fifa.df2[, c('preferred_foot', 'real_face')], unclass)
fifa.df2 <- subset(fifa.df2, select = -c(value_eur ))
feature2 <- subset(fifa.df2, select = -c(wage_eur))

#Univariate Selection
filterCtrl <- sbfControl(functions = rfSBF, method = "repeatedcv", repeats = 2)
rfWithFilter <- sbf(x= feature2,y= fifa.df2$wage_eur, sbfControl = filterCtrl)
rfWithFilter

#PCA  -  with numeric data
install.packages('ggplot2')
library(ggcorrplot)
corr_matrix <- cor(fifa.df)
ggcorrplot(corr_matrix)