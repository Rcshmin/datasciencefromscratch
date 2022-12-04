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

calculate_overall_entropy <- function(data_below, data_above){
  #Proportion of samples in left and right children
  n = length(data_below) + length(data_above)
  p_data_below = length(data_below)/n
  p_data_above = length(data_above)/n
  #Calculate overall entropy
  overall_entropy = (p_data_below*get_entropy(pull(data_below[, -1]))) +
    (p_data_above*get_entropy(pull(data_above[, -1])))
  return(overall_entropy)
}

determine_best_split <- function(data, potential_splits){
  #Initialize overall entropy and col 
  running_entropy = 9999
  best_split_value = 0
  best_split_column = ""
  #Find best entropy over potential splits
  for(j in 1:ncol(potential_splits)){
    for(i in unique(potential_splits[, j])){
      mask_val = i 
      mask_col = j
      splits = split_data(data = data, split_column = mask_col, split_value = mask_val)
      relative_entropy = calculate_overall_entropy(data_above = splits[[1]], data_below = splits[[2]])
      if(relative_entropy < running_entropy){
        running_entropy = relative_entropy
        best_split_value = mask_val
        best_split_column = colnames(potential_splits)[j]
      } else {
        next(i)
      }
    }
  }
  return(list(best_split_column, best_split_value))
}

decision_tree_algorithm <- function(df, 
                                    counter = 1, 
                                    min_samples, 
                                    max_depth, is.child = "root"){
  data = df
  #Check whether stopping conditions have been violated
  if(any(check_purity(data), nrow(data) < min_samples, (counter - 1) == max_depth)){
    classification = classify_data(data)
    return(print(paste(classification, is.child, counter)))
  } else {
    #Recursive part
    
    #Helper functions
    potential_splits = get_potential_splits(data)
    split_g = determine_best_split(data, potential_splits)
    split_column = split_g[[1]]
    split_value = split_g[[2]]
    data_g = split_data(data, split_column, split_value)
    data_above = data_g[[1]]
    data_below = data_g[[2]]
    print(paste(split_column, split_value, is.child, counter))
    #Find the answers
    yes_answer = decision_tree_algorithm(df = data_below, counter = counter + 1, min_samples, max_depth, is.child = "yes <=")
    no_answer = decision_tree_algorithm(df = data_above, counter = counter + 1, min_samples, max_depth, is.child = "no >")
    
  }
  
}

decision_tree <- function(df, min_samples, max_depth){
  
  #Store decisions and reformat
  decisions = capture.output(decision_tree_algorithm(df = df, min_samples = min_samples, max_depth = max_depth), append = F)
  decisions_df = strsplit(decisions, " ")
  decisions_cl = sapply(decisions_df, function(x) gsub("\"", "", x))
  
  #Make list of equal length
  for(i in 1:length(decisions_cl)){
    if(length(decisions_cl[[i]]) != 6){
      if(i == 1){
        decisions_cl[[i]] = append(decisions_cl[[i]], NA, after = 4)
      } else {
        decisions_cl[[i]] = append(decisions_cl[[i]], NA, after = 2)
      }} else { next(i) }}
  
  #Convert to df and reformat again
  decisions_df = as.data.frame(decisions_cl)
  decisions_t = as.data.frame(t(decisions_df))
  decisions_x = decisions_t[, -1]
  row.names(decisions_x) = 1:nrow(decisions_x)
  colnames(decisions_x) = c("node", "split.val", "is.child", "split", "depth")
  
  #Sort the df
  decisions_x = decisions_x[order(decisions_x$depth), ]
  
  return(decisions_x)
}


