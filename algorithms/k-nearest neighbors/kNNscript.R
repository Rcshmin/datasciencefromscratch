get_euclid <- function(p, q){
  #Check for same length
  if(length(p) != length(q)) return("Error, unequal length!")
  #Calculate distance
  distance = sqrt(sum((q-p)^2))
  return(distance)
}

get_cosine <- function(p, q){
  #Check for same length
  if(length(p) != length(q)) return("Error, unequal length!")
  #Calculate cosine distance
  cos_dot = sum(p*q)
  p_mag = sqrt(sum(p^2))
  q_mag = sqrt(sum(q^2))
  cos_dis = 1 - cos_dot/(p_mag*q_mag)
  return(round(cos_dis, digits = 10))
}

get_majority_vote <- function(labels){
  #Vote frequencies and max
  votefreq = as.data.frame(table(labels))
  votemax = max(votefreq[, 2])
  voteties = sum(votefreq[, 2] == votemax)
  #First case for no ties, second for ties
  if(voteties == 1){
    return(votefreq[votefreq[, 2] == votemax, ][1,1])
  } else {
    #Reduce label length 
    i = length(labels)
    while(voteties != 1){
      i = i - 1
      labels  = labels[1:i]
      votefreq = as.data.frame(table(labels))
      votemax = max(votefreq[, 2])
      voteties = sum(votefreq[, 2] == votemax)
    }
    return(votefreq[votefreq[, 2] == votemax, ][1,1])
  }
}

knn_classify <- function(df, k, new_point, type){
  #Check the new point is of same length
  if(length(new_point) != ncol(df) - 1) return("Error, unequal length!")
  #Calculate the distances and order them
  df_euclid = df[, (ncol(df) - 1):ncol(df)] 
  df_points = as.data.frame(df[, 1:(ncol(df)-1)])
  for(i in 1:nrow(df_euclid)){
    df_euclid[i, 1] = get_euclid(p = new_point, 
                                 q = as.numeric(df_points[i, ]))
  }
  df_order = df_euclid[order(df_euclid[,1]), ]
  #Return the k-closest neighbors
  df_knn = df_order[1:k, ]
  labels_k = df_knn[, 2]
  #The dominating label
  dom_label = get_majority_vote(labels = labels_k)
  return(dom_label)
}

knn_regress <- function(df, k, new_point){
  #Check the new point is of same length
  if(length(new_point) != ncol(df) - 1) return("Error, unequal length!")
  #Calculate the distances and order them
  df_euclid = df[, (ncol(df) - 1):ncol(df)] 
  df_points = as.data.frame(df[, 1:(ncol(df)-1)])
  for(i in 1:nrow(df_euclid)){
    df_euclid[i, 1] = get_euclid(p = new_point, 
                                 q = as.numeric(df_points[i, ]))
  }
  df_order = df_euclid[order(df_euclid[,1]), ]
  #Return the k-closest neighbors
  df_knn = df_order[1:k, ]
  #The average value
  regress_val = mean(df_knn[, 2])
  return(regress_val)
}