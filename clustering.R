# Cluster Analysis

setwd('~/Desktop/r_projects/prac_ds_r')

url = 'https://raw.githubusercontent.com/WinVector/zmPDSwR/master/Protein/protein.txt' 

protein <- read.table(url, sep='\t', header=T)

summary(protein)

# rescaling the dataset
vars.to.use <- colnames(protein)[-1]
pmatrix <- scale(protein[, vars.to.use])

# The scale() function annotates its output with two attributes—scaled:center
# returns the mean values of all the columns, and scaled:scale returns the
# standard deviations. Store these away so you can “unscale” the data later
pcenter <- attr(pmatrix, 'scaled:center')
pscale <- attr(pmatrix, 'scaled:scale')

# Hierarchical clustering
# Create the distance matrix
d <- dist(pmatrix, method='euclidean')

# Do the clustering.
pfit <- hclust(d, method='ward.D')

# Plot the dendrogram.
plot(pfit, labels=protein$Country)
rect.hclust(pfit, k=5)

# Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)

print_clusters <- function(labels, k) {
  for (i in 1:k) {
    print(paste('cluster', i))
    print(protein[labels==i, c("Country","RedMeat","Fish","Fr.Veg")])
  }
}

print_clusters(groups, 5)

# Projecting the clusters on the first two principal components
library(ggplot2)

# Calculate the principal components of the data.
princ <- prcomp(pmatrix)

nComp <- 2

# The predict() function will rotate the data into the space described by
# the principal components. We only want the projection on the first two axes.
project <- predict(princ, newdata=pmatrix)[, 1:nComp]

project.plus <- cbind(as.data.frame(project),
                      cluster=as.factor(groups),
                      country=protein$Country)

ggplot(data=project.plus, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape=cluster)) +
  geom_text(aes(label=country), hjust=0, vjust=1)

# Running clusterboot() to check clustering stablity 
# As a rule of thumb, clusters with a stability value less than 0.6 should be 
# considered unstable Values between 0.6 and 0.75 indicate that the cluster 
# is measuring a pattern in the data, but there isn’t high certainty
# about which points should be clustered together. Clusters with stability values
# above about 0.85 can be considered highly stable (they’re likely to be real clusters).

library(fpc)

# Set the desired number of clusters
kbest.p <- 5

cboot.hclust <-clusterboot(pmatrix, clustermethod=hclustCBI,
                           method='ward.D', k=kbest.p)

summary(cboot.hclust$result)

# cboot.hclust$result$partition returns a vector of cluster labels.
groups <- cboot.hclust$result$partition
groups

print_clusters(groups, kbest.p)

# The vector of cluster stabilities.
cboot.hclust$bootmean

# The count of how many times each cluster was dissolved. By default clusterboot()
# runs 100 bootstrap iterations.
cboot.hclust$bootbrd

# The clusterboot() results show that the cluster of countries with high fish 
# consumption (cluster 4) is highly stable. Cluster 3 is less stable

# k-means clustering 
pclusters <- kmeans(pmatrix, kbest.p, nstart=100, iter.max=100)
summary(pclusters)

pclusters$centers

pclusters$size

groups <- pclusters$cluster

print_clusters(groups, kbest.p)

# To run kmeans(), you must know k. The fpc package (the same package that has
# clusterboot()) has a function called kmeansruns() that calls kmeans() over 
# a range of k and estimates the best k.

# kmeansruns() has two criteria: the Calinski-Harabasz Index ("ch"), and the 
# average silhouette width ("asw")

clustering.ch <- kmeansruns(pmatrix, krange=1:10, criterion="ch")
clustering.ch$bestk
# The CH criterion picks two clusters.

clustering.asw <- kmeansruns(pmatrix, krange=1:10, criterion="asw")
clustering.asw$bestk
Average
# The silhouette width picks 3 clusters.





