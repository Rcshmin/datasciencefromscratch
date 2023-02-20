library(tidyverse)

#we want three clusters for iris for the below variables
ggplot(data = iris, aes(x = Sepal.Length, y = Petal.Width, color = Species)) +
  geom_point()

#helpers for getting the distance 

get_euclid =
  function(p,q){
    distance = sqrt(sum((p-q)^2))
    return(distance)
  }

get_distance =
  function(data, dist.met, centroid){
    df = data
    distance = c()
    for(i in 1:nrow(df)){
      obs = as.numeric(df[i, ])
      obs.dist = dist.met(obs, centroid)
      distance[i] = obs.dist
      next(i)
    }
    return(distance)
  }

get_distance(data = iris[,c(1,2,3)], dist.met = get_euclid, centroid = c(1,2,3))

#calculate distance for each cluster

get_cluster_distance =
  function(clusters, data, dist.met, k){
    clusters_dist = data.frame(matrix(nrow = nrow(data), ncol = k))
    for(i in 1:k){
      clusters_dist[, i] = get_distance(data, dist.met, centroid = clusters[[i]])
      next(i)
    }
    return(clusters_dist)
  }

random_clust = list(c(5, 0.7,1), c(7, 0.3,4), c(8,2.5,5))

test_dist = get_cluster_distance(clusters = random_clust, data = iris[,c(1,2,3)], dist.met = get_euclid, k = 3)

#assign each point to a cluster

assign_cluster =
  function(clusters, data, dist.met, k){
    clusters_dist = get_cluster_distance(clusters, data, dist.met, k)
    cluster_label = c()
    for(i in 1:nrow(clusters_dist)){
      obs = as.numeric(clusters_dist[i, ])
      min.dist = min(obs)
      index.min = which(obs == min.dist)
      if(length(index.min) > 1){
        index.min = sample(x = index.min, size = 1)
      }
      cluster_label[i] = index.min
      next(i)
    }
    return(cluster_label)
  }

assign_cluster(clusters = random_clust, data = iris[,c(1,2,3)], dist.met = get_euclid, k = 3)

#the main algorithm

kmeans =
  function(data, dist.met, k){
    #randomly initialize centroids; rp method
    clusters = list()
    for(i in 1:k){
      clusters[[i]] = as.numeric(data[sample(x = 1:nrow(data), size = 1), ])
    }
    #copy data and add name
    dat = data
    dat[, ncol(data)+1] = double(length = nrow(data))
    colnames(dat)[ncol(data)+1] = "cluster"
    #assign dummy values
    cluster_initial = 0
    cluster_next = 1
    #itterate until convergence  
    while(any(cluster_initial != cluster_next)){
      #assign cluster
      cluster_initial = assign_cluster(clusters, data = dat[,-ncol(dat)], dist.met, k)
      #add clusters
      dat$cluster = cluster_initial
      #update centroid
      for(i in 1:k){
        obs.clus = subset(dat, dat$cluster == i)
        cen.new = c()
        for(j in 1:ncol(data)){
          average.j = mean(obs.clus[, j])
          cen.new[j] = average.j
          next(j)
        }
        clusters[[i]] = cen.new
        next(i)
      }
      cluster_next = assign_cluster(clusters, data = dat[,-ncol(dat)], dist.met, k)
      dat$cluster = cluster_next
    }
    return(dat)
  }

#testing the algorithm

set.seed(2372)
iris1 = iris[sample(1:nrow(iris), size = nrow(iris)),]
iris2 = kmeans(data = iris1[,-5], dist.met = get_euclid, k = 3)
iris3 = cbind(iris2, iris1$Species)
for(i in 1:nrow(iris3)){
  if(iris3$cluster[i] == 1){
    iris3$cluster[i] = "setosa"
  } else if(iris3$cluster[i] == 2){
    iris3$cluster[i] = "versicolor"
  } else {
    iris3$cluster[i] = "virginica"
  }
}
label.logics = iris3[, 5] == iris3[, 6]
print(length(which(label.logics == TRUE))/nrow(iris))


