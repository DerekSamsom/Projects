library(cluster)
library(rattle.data)
library(NbClust)

# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

#install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package = "rattle.data")
head(wine)


# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

wine_2 <- wine
wine_2[,1] <- NULL
wine_2 <- scale(wine_2)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc = 15, seed = 1234){
	              wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
               	      for(i in 2:nc){ 
		          set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers = i)$withinss)}
	                
		          plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
	                        ylab = "Within groups sum of squares")
	   }

wssplot(wine_2)

# Exercise 2:
#   * How many clusters does this method suggest?
# This method suggests using 3 clusters, as this is where there is a bend
# in the curve.

#   * Why does this method work? What's the intuition behind it?
# A large drop of the within-group sum of squares when increasing the number
# of clusters shows that the additional cluster is better 
# describing the data. When the curve bends, adding additional clusters are no
# longer causing much decrease in the within-group sum of squares.


#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

set.seed(1234)
nc <- NbClust(wine_2, min.nc = 2, max.nc = 15, method = "kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab = "Numer of Clusters", ylab = "Number of Criteria",
		            main = "Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# This method also suggests 3 clusters based on majority rule.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit_km <- kmeans(wine_2, centers = 3, nstart = 25)

# Now we want to evaluate how well this clustering does.
fit_km$size
fit_km$centers

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

ct_km <- table(wine$Type, fit_km$cluster)
ct_km

# 172/178 are correctly classified, or 96.6%. I would consider this a good
# clustering.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?

clusplot(wine_2, clus = fit_km$cluster)

# I consider this a good clustering. Visuallt the point in the clusplot match
# up well with the ellipses representing the clusters.
