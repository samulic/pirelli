## This file contains very basic preprocessing steps which include
# - YEAR as ordered factor
# - PRODUCT_ID as character
# - Remove zero variance attributes (columns that only have one unique value)
## The processed dataset is saved as r data object under the name of data

library(dplyr)
d <- read.csv("input/sales_dataset.csv")
str(d)
# Since sales from the last year (9) are much less, we believe it is the actual year=2018
# so we add 2009 to match the correct (may be) year and make it an ordered factor
d$YEAR <- as.ordered(d$YEAR + 2009) 
#d$YEAR <- recode(d$YEAR, "9='2018'; 8='2017'; 7='2016'; 6='2015'; 5='2014'; 4='2013'; 3='2012'; 2='2011'; 1='2010'", as.factor = T)
d$PRODUCT_ID <- as.character(d$PRODUCT_ID)
# Factorize categorical attributes
categorical_var <- names(d %>% select(contains("CATEGORICAL"))) # find columns' names
d[, categorical_var] <- lapply(d[, categorical_var], as.factor)
# Find and remove zero variance columns
nzv <- caret::nearZeroVar(d, saveMetrics = TRUE)
head(nzv[order(nzv$percentUnique, decreasing = FALSE), ], n = 20)
(zerovar <- rownames(nzv[nzv$zeroVar == T,]))
d <- select(d, -one_of(zerovar))
Hmisc::describe(d)

data <- d
save(data, file = "input/data.rdata")
