
train_test_split <- function(data, test){
  #Store data
  t_data = data
  #Assign amount of samples
  if(test < 1){
    sample_test = ceiling(test*nrow(t_data))
    sample_train = nrow(t_data) - sample_test
  } else {
    sample_test = test
    sample_train = nrow(t_data) - sample_test
  }
  #Shuffle data 
  t_data = t_data[sample(nrow(t_data)), ]
  row.names(t_data) = 1:nrow(t_data)
  #Assign partition and indices
  indices = sample(x = sample(1:nrow(t_data), size = nrow(t_data), replace = FALSE))
  primary = rep(x = c("train"), sample_train)
  secondary = rep(x = c("test"), sample_test)
  total = append(primary, secondary)
  t_data$my.folds[indices] = total
  return(t_data)
}

k_fold_split <- function(k, data){
  k_data = data
  #Shuffle the data
  k_data = k_data[sample(nrow(k_data)), ]
  row.names(k_data) = 1:nrow(k_data)
  #Assign partition number
  if(nrow(data) %% k == 0){
    #Vector of randomized indices
    indices = sample(x = sample(1:nrow(k_data)), size = nrow(k_data), replace = FALSE)
    #Assigning indices
    k_data$my.folds[indices] = rep(x = c(1:k), rep(nrow(k_data)/k, k))
  } 
  else
  {
    #If %% != 0, then randomize the remainder, after equally distributing the floor
    indices = sample(x = sample(1:nrow(k_data)), size = nrow(k_data), replace = FALSE)
    primary = rep(x = c(1:k), rep(floor(nrow(k_data)/k), k))
    secondary = sample(x = c(1:k), replace = FALSE, size = nrow(k_data) %% k)
    total = append(primary, secondary)
    k_data$my.folds[indices] = total
  }
  return(k_data)
}

k_fold_arrange <- function(k, data){
  #Call previous function
  k_data = k_fold_split(k, data)
  #Initialize list
  k_list = vector("list", k)
  #Fill the list
  for(i in 1:k){
    k_list[[i]] = k_data
    test_indices = which(k_data$my.folds == i)
    train_indices = which(k_data$my.folds != i)
    k_list[[i]]$my.folds[test_indices] = "testing"
    k_list[[i]]$my.folds[train_indices] = "training"
    next(i)
  }
  return(k_list)
}