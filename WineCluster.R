#dependencies
install.packages("cluster")
install.packages("NbClust")
install.packages('factoextra')
install.packages("ClusterR")
install.packages("readxl")

library("cluster")
library("NbClust")
library("factoextra")
library("ClusterR")
library("readxl")
#Importing dataset 
data <- read_excel("Whitewine.xlsx")

# Get some metrics about the variables
summary(data)
boxplot(data)

oldpar = par(mfrow = c(2,6))
for ( i in 1:11 ) {
  boxplot(data[[i]])
  mtext(names(data)[i], cex = 0.8, side = 1, line = 2)
}


#remove outliers 
#Refered to https://universeofdatascience.com/how-to-remove-outliers-from-data-in-r/#:~:text=There%20exist%20two%20ways%20of,find%20upper%20limit%20for%20outliers.


remove_outliers <- function(name) {
  
  quartile <- quantile(data.clean[,name], probs=c(.25, .75), na.rm = FALSE)
  iqr <- IQR(data.clean[,name])
  
  upper_range <-  quartile[2]+1.5*iqr 
  lower_range <- quartile[1]-1.5*iqr 
  
  return(subset(data.clean, data.clean[,name] > lower_range 
                & data.clean[,name] < upper_range))
}

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Normalize the data except the quality
data.normalized <- as.data.frame(lapply(data[, 1:11], normalize))

# Add the quality column to the normalized data
data.normalized$quality <- data$quality

# Remove outliers
data.clean <- data.normalized

data.clean <- remove_outliers("fixed.acidity")
data.clean <- remove_outliers("volatile.acidity")
data.clean <- remove_outliers("citric.acid")
data.clean <- remove_outliers("residual.sugar")
data.clean <- remove_outliers("chlorides")
data.clean <- remove_outliers("free.sulfur.dioxide")
data.clean <- remove_outliers("total.sulfur.dioxide")
data.clean <- remove_outliers("density")
data.clean <- remove_outliers("pH")
data.clean <- remove_outliers("sulphates")
data.clean <- remove_outliers("alcohol")

# matrix created with 2 rows and 6 columns
boxplot.panels = par(mfrow = c(2,6)) 

# visualizing the data after removal of outliers  
for (i in 1:11) { 
  boxplot(data.clean[[i]]) 
  mtext(names(data.clean)[i], cex = 0.8, side = 1, line = 2) } 

# NbClust Method
#set.seed(23)
clusters.nb <- NbClust(data = data.clean[1:11], method = "kmeans")

#calclulate how many clusters wss
# Elbow Method
fviz_nbclust(data.clean[1:11], kmeans, method = 'wss')
fviz_nbclust(data.transform, kmeans, method = 'wss')

# Silhouette Method
fviz_nbclust(data.clean[1:11], kmeans, method = 'silhouette')
fviz_nbclust(data.transform, kmeans, method = 'silhouette')


#Analysis for each 
# Create clusters for k = 2, k = 3 and k = 4
kmeans.2 <- kmeans(data.clean[, 1:11], centers = 2)
kmeans.3 <- kmeans(data.clean[, 1:11], centers = 3)
kmeans.4 <- kmeans(data.clean[, 1:11], centers = 4)

# Show the clusters
clusplot(data.clean, kmeans.2$cluster, main='2D representation of the Cluster', color=TRUE, shade=TRUE)
clusplot(data.clean, kmeans.3$cluster, main='2D representation of the Cluster', color=TRUE, shade=TRUE)
clusplot(data.clean, kmeans.4$cluster, main='2D representation of the Cluster', color=TRUE, shade=TRUE)

# Confusion matrix
table(kmeans.2$cluster, data.clean$quality)
table(kmeans.3$cluster, data.clean$quality)
table(kmeans.4$cluster, data.clean$quality)

# BSS / TSS

kmeans.2$betweenss/kmeans.2$totss * 100
kmeans.3$betweenss/kmeans.3$totss * 100
kmeans.4$betweenss/kmeans.4$totss * 100


data.pca = prcomp(data.clean[1:11], center = TRUE, scale = FALSE)
summary(data.pca)

# Take only the PCs with a cumulative score > 96%
data.transform <- as.data.frame(-data.pca$x[,1:2])

kmeans.2_pca <- kmeans(data.transform, centers = 2, nstart = 50)
kmeans.3_pca <- kmeans(data.transform, centers = 3, nstart = 50)
kmeans.4_pca <- kmeans(data.transform, centers = 4, nstart = 50)

fviz_cluster(kmeans.2_pca, data = data.transform, geom = "point")
fviz_cluster(kmeans.3_pca, data = data.transform, geom = "point")
fviz_cluster(kmeans.4_pca, data = data.transform, geom = "point")

table(kmeans.2_pca$cluster, data.clean$quality)


