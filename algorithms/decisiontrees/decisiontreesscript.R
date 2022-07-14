get_entropy <- function(x){
#Assume x is factor of labels
if(length(x) == 0) return(0)
weights = table(x)/length(x)
info_content = -weights*log2(weights)
entropy = sum(info_content)
return(entropy)
}

get_gini_impurity <- function(x){
#Assume x is a factor with labels
if(length(x) == 0) return(0)
weights = table(x)/length(x)
weights_squared = weights^2
sum_of_squares = sum(weights_squared)
gini = 1 - sum_of_squares
return(gini)
}

get_train_test <- function(df, train_size){
observations = 1:nrow(df)
#Option for a proportion or number
if(train_size < 1){
  test_size_f = round(train_size*nrow(df))
} else {
  test_size_f = train_size
}
#Get index of train values
train_index = sample(observations, size = test_size_f)
test_observations = nrow(df) - length(train_index)
#Get index of test values
test_index = double(length = length(observations))
for(i in 1:length(observations)){
  if(any(observations[i] == train_index)){
    next(i)
  } else {
    test_index[i] = observations[i]
  }
}
test_index_f = c(subset(test_index, test_index > 0))
#Create df's from index values
train_df = df[train_index, ]
test_df = df[test_index_f, ]
return(list(train_df, test_df))
}

check_purity <- function(data){
  #Get unique labels
  labels = length(unique(pull(data[, -1])))
  #Check if there is only one
  ifelse(labels == 1, return(TRUE), return(FALSE))
}

classify_data <- function(data){
  #Get labels
  get_labels = pull(data[, -1])
  #Get label frequency and max
  label_freq = table(get_labels)
  label_freq_a = as.data.frame(label_freq)
  label_dom = max(label_freq)
  #Get classification
  for(i in 1:nrow(label_freq_a)){
    if(label_freq_a$Freq[i] == label_dom){
      classification = as.character(label_freq_a$get_labels[i])
    } else {
      next(i)
    }
  }
  return(classification)
}

split_data <- function(data, split_column, split_value){
  split_c = data[[split_column]]
  #Filter the data into above and below
  data_below = data[split_c <= split_value, ]
  data_above = data[split_c > split_value, ]
  return(list(data_above, data_below))
}

get_potential_splits <- function(data){
  #Sorting stage
  data = data
  col_n = ncol(data) - 1
  for(i in 1:col_n){
    data_i = sort(data[, i])
    data[, i] = data_i
  }
  #Creating the splits
  dat = data[0, ]
  for(j in 1:col_n){
    for(i in 2:nrow(data)){
      curr_val = data[i, j]
      previous_val = data[(i-1), j]
      potential_val = (curr_val + previous_val)/2
      dat[(i-1), j] = potential_val
    }
  }
  dat[nrow(dat)+1, ] = data[nrow(data), ]
  dat = dat[, 1:col_n]
  potential_splits = dat
  return(potential_splits)
}



