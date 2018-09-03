options(scipen = 999)
library(tidyverse)
library(funModeling)

d <- read_csv(file = "input/sales_dataset.csv")
head(d)
# PRODUCT_ID = modello di pneumatico
# SALES = vendite di PRODUCT_ID
# BRANDS = vendite delle automobile che possono montare tale PRODUCT_ID
# CATEGORICAL = caratteristiche di PRODUCT_ID
status <- df_status(d, print_results = F)
status %>% arrange(desc(unique))

summary <- data.frame(profiling_num(d))
custom_profiling_table <- summary %>% select(variable, mean, std_dev, skewness, p_01, p_99, range_80) 
(my_profiling_table <- left_join(custom_profiling_table, select(status, variable, p_zeros, unique), 
                                by = "variable") %>% arrange(unique))
# Ok there are some BRANDS that never sell anything, we remove them
vars_to_remove <- my_profiling_table %>% filter(unique == 1) %>% .$variable
d <- select(d, -one_of(vars_to_remove))
# Not a nice code but, recompute table...
summary <- data.frame(profiling_num(d))
custom_profiling_table <- summary %>% select(variable, mean, std_dev, skewness, p_01, p_99, range_80) 
(my_profiling_table <- left_join(custom_profiling_table, select(status, variable, p_zeros, unique), 
                                 by = "variable") %>% arrange(unique))
# Variabili che hanno molti zeri (non e' una selezione rigorosa, e' solo per snellire il plot di correlazione)
near_zero_var <- filter(my_profiling_table, p_zeros > 99) %>% .$variable

corrplot::corrplot.mixed(cor(select(d, -one_of(near_zero_var))))

brands_var <- names(d %>% select(contains("BRAND")))
categorical_var <- names(d %>% select(contains("CATEGORICAL")))
###### SALES ######
# Come si distribuiscono le vendite per ogni anno?
par(mfrow=c(3,3))
for(i in unique(d$YEAR)){
  boxplot(log(d[d$YEAR == i, "SALES"]), main = i, col = "lightblue", 
          xlab = paste("Year: ", i, sep = ""), ylab = 'log(Sales)')
}
par(mfrow=c(1, 1))
# Last year, year 9, has fewer sales, maybe it is not a full 12 months year. Let's count obs
d %>% group_by(YEAR) %>% summarise(observations = n(), 
                                   unique_products = length(unique(PRODUCT_ID)), 
                                   avg_sales = mean(SALES),
                                   sum_sales = sum(SALES))
# Ok we have roughly the same number observation but that doesn't mean the sales were counted over 12 months
# No way to know :( ...? But we strongly believe it is not a full year of sales.
# We notice that starting from year 5 we have a few repetitions of the same product ID.
# Let's inspect the duplicated product ID
(duplicates <- group_by(d, YEAR, PRODUCT_ID) %>% summarise(n = n()) %>% filter(n > 1))
duplicates <- d[d$YEAR >= min(duplicates$YEAR) & d$PRODUCT_ID %in% unique(duplicates$PRODUCT_ID), ]

###### BRAND ######
# BRAND should be the sales of the cars' brands that can use the PRODUCT_ID
# Let's check the correlation degree between the sum of cars' sales and the tires' sales
brands_sum <- rowSums(d[,names(d %>% select(contains("BRAND")))])
cor(d$SALES, brands_sum) # not much correlation

# How do the brands' sales distribute ?
par(mfrow=c(4,4))
for(i in setdiff(brands_var, near_zero_var)) { # variable brands
  boxplot(d[, i], main = i, col = "lightblue", ylab = "Sales")
}
par(mfrow=c(3,6))
for(i in near_zero_var) {
  boxplot(d[, i], main = i, col = "lightblue",
          ylab = 'Sales') # not variable brands
}
par(mfrow=c(1,1))

# Let's build a linear model to predict sales from brands
formula <- paste("SALES ~ ", paste(brands_var, collapse = " + "), sep = "")
fit <- lm(data = d, as.formula(formula))
summary(fit)



###### CATEGORICAL ###### or tires' characteristic
describe(d[,categorical_var])
d[d$"CATEGORICAL_1" == 9,]
d[d$"CATEGORICAL_2" == 19,]
d[d$"CATEGORICAL_2" == 18,]
d[d$"CATEGORICAL_3" == 8,]
prop.table(table(d$CATEGORICAL_4))
d[d$"CATEGORICAL_6" == 7,]
d[d$"CATEGORICAL_6" == 5,]
d[d$"CATEGORICAL_7" == 1,]

# Do identical products have the same categorical values?
id_cat <- group_by(d, PRODUCT_ID, CATEGORICAL_0, CATEGORICAL_2, CATEGORICAL_3, CATEGORICAL_4, CATEGORICAL_5, CATEGORICAL_6, CATEGORICAL_7, CATEGORICAL_8) %>%
  summarise(years = n())
id_cat[duplicated(id_cat$PRODUCT_ID),] # Yes, except three products
# These are the same we already discovered as duplicates
d[d$PRODUCT_ID %in% unique(duplicates$PRODUCT_ID), c("PRODUCT_ID", categorical_var)]


par(mfrow=c(3,3))
for (cat in categorical_var) {
  formula <- as.formula(paste("SALES ~", cat))
  boxplot(formula, data = d, main = cat, col = "lightblue", ylab = "Sales")
}
# Ok this is not useful since we know nothing about categorical and their values.


