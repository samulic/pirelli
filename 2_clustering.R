library(tidyverse)
library(klaR) #import library to use kmodes function for categorical attributes'
library(clustMixType) #import library to use kproto function for mixed attributes' type

load("input/data.rdata")
# Get the column names as vector of characters, useful later
categorical_var <- names(data %>% select(contains("CATEGORICAL")))
brands_var <- names(data %>% select(contains("BRAND")))

# We want to reduce the number of PRODUCT_ID levels (718)
# To do so we try to cluster only CATEGORICAL attributes at first
# Remember that CATEGORICAL contains the characteristics of PRODUCT_ID (tires)
# Therefore, as we have already seen in 0_exploration.r, same product --> same categorical (with three exceptions)
x <- data[, categorical_var]
funModeling::df_status(x, print_results = F) %>% arrange(unique)

ks <- 2:200 # number of clusters we want to try (200 clusters is overkill)
smw <- numeric(length(ks)) # vector for the SimpleMatching_Within distance
for (i in seq_along(ks)) {
  cat("n. of clusters", i+1, "\n")
  set.seed(456)
  smw[i] <- sum(kmodes(x, modes = ks[i], iter.max = 100, fast = TRUE)$withindiff) #sum over each cluster's within distances
}
# Plot results, the distance within in function of the number of clusters
ggplot(mapping = aes(x = ks, y = smw)) +
  geom_line() +
  xlab("Number of clusters") +
  ylab("Sum of within-cluster simple-matching distances") +
  ggtitle("CATEGORICAL clustering", subtitle = "Look for an elbow")


# Can this simple-matching distance be reliable, or does it make any sense, if the levels of the categories 
# are highly imbalanced? Don't think so, so let's use the weighted option, in which the distance is weighted
# by the frequencies of the categories in data (Huang 1997 for more details)
ks <- 2:150 # number of clusters we want to try
wmw <- numeric(length(ks)) # vector for the weighted-matching distances
for (i in seq_along(ks)) {
  cat("n. of clusters", i+1, "\n")
  set.seed(654)
  wmw[i] <- sum(kmodes(x, modes = ks[i], iter.max = 100, fast = T, weighted = TRUE)$withindiff) 
}
ggplot(mapping = aes(x = ks, y = wmw)) +
  geom_line() +
  xlab("Number of clusters") +
  ylab("Sum of within-cluster weighted-matching distances") +
  ggtitle("CATEGORICAL clustering (weighted)", subtitle = "Look for an elbow") +
  geom_vline(aes(xintercept = c(33, 49, 69), colour = "red", alpha = 0.5)) + theme(legend.position="none")
# 33, 49 and 69 could be a nice number of cluster
# Plot the difference in -gained distance for adding one cluster
ggplot(mapping = aes(x = ks[-1], y = -wmw[-1] + wmw[-length(wmw)])) +
  geom_col() +
  xlab("Number of clusters") +
  ylab("Decrement in WM_within") +
  ggtitle("CATEGORICAL clustering (weighted)", subtitle = "Look for a spike") +
  geom_vline(aes(xintercept = c(33, 49, 69), colour = "red", alpha = 0.5)) + theme(legend.position="none")


# Let us include numeric variables
x.num <- scale(data[, brands_var])
x.cat <- x
x <- cbind(x.cat, x.num)

# We have a new parameter "lambda" which is the trade off between Euclidean distance of numeric variables 
# and simple matching coefficient between categorical variables--> d(x,y)= d_euclid(x,y) + λ * d_simple_matching(x,y)
# So, shall we give more importance to categorical or to numeric (i.e. BRANDS --> cars' sales) ? 
# I would say categorical, since they are directly related to product_id because they define its characteristics
# Let's consider categoricals 10 times more important than numeric

ks <- 2:50 # number of clusters we want to try
mmw <- numeric(length(ks)) # vector for the mixed weighted-matching distances
for (i in seq_along(ks)) {
  cat("n. of clusters", i+1, "\n")
  set.seed(45654)
  mmw[i] <- kproto(x, k = ks[i], lambda = 10, iter.max = 100, keep.data = F)$tot.withinss
}
ggplot(mapping = aes(x = ks, y = mmw)) +
  geom_line() +
  xlab("Number of clusters") +
  ylab("Sum of within-cluster mixed-matching distances") +
  ggtitle("Mixed attributes' clustering", subtitle = "Lambda = 10") +
  geom_vline(aes(xintercept = 4, colour = "red", alpha = 0.5)) + theme(legend.position="none")
