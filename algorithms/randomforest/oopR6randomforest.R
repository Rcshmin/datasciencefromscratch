#Random forest

#Import libraries

library(tidyverse)
library(R6)
library(devtools)

#Import train-test split function from my github
source_url("https://raw.githubusercontent.com/Rcshmin/datasciencefromscratch/main/algorithms/kfoldcv/kfoldscript.R")

##### Helper functions for random forest classifier #####

# Bootstrapping #

get_bootstrap = function(train_df, boot_samp){
  #If bootstrap samples is not provided, randomize
  if(is.null(boot_samp)){
    boot_samp = sample(x = 1:nrow(train_df), size = 1)
  } else {
    boot_samp = boot_samp
  }
  #Sample indices with replacement; replace = T, creates decimal indices as row names cannot be same
  indices = sample(x = 1:nrow(train_df), size = boot_samp, replace = TRUE)
  #Return bootstrapped data
  boot_df = train_df[indices, ]
  return(boot_df)
}

# Altered potential splits #

# This subspace method is done at the node level, where the first node is a subset of the original df features, and resultant nodes are a subset of the parent nodes features

get_potential_splits =
  function(data, subspace){
    #Remove target column
    data_c = data
    col_n = ncol(data_c) - 1
    #Subspacing
    if(subspace == TRUE){
      col_s = sample(x = 1:col_n, size = sample(x = 1:col_n, size = 1), replace = FALSE)
    } else {
      col_s = 1:col_n
    }
    data_c = as.data.frame(data[,col_s])
    #Ensuring stays as data frame when one column
    if(ncol(data_c) == 1){
      colnames(data_c)[[1]] = colnames(data)[[col_s]]
    }
    return(data_c)
  }

##### Helper functions for random forest regressor #####

# Bootstrapping #

# This subspace method is done at the tree level. In other words the whole tree is built on one subset of the original df features 

# Also we implement subspacing in the same function as bootstrapping here

get_bootstrap_subspace = 
  function(train_df, boot_samp, subspace){
  #If bootstrap samples is not provided, randomize
  if(is.null(boot_samp)){
    boot_samp = sample(x = 1:nrow(train_df), size = 1)
  } else {
    boot_samp = boot_samp
  }
  #Sample indices with replacement; replace = T, creates decimal indices as row names cannot be same
  indices = sample(x = 1:nrow(train_df), size = boot_samp, replace = TRUE)
  #Get bootsrapped data
  boot_df = train_df[indices, ]
  #Subspacing
  if(subspace == TRUE){
    col_n = ncol(boot_df)-1
    col_s = sample(x = 1:col_n, size = sample(x = 1:col_n, size = 1), replace = FALSE)
    col_final = append(col_s, ncol(boot_df))
    return(boot_df[,col_final])
  } else {
    return(boot_df)
  }
}

get_bootstrap_subspace(train_df = iris[, -5], boot_samp = NULL, subspace = TRUE)

# Altered potential splits #

get_potential_splits =
  function(data, subspace){
    #Remove target column
    data_c = data
    col_n = ncol(data_c) - 1
    col_s = 1:col_n
    data_c = as.data.frame(data[,col_s])
    #Sorting
    for(i in 1:ncol(data_c)){
      data_i = sort(data_c[, i])
      data_c[, i] = data_i
    }
    #Initialize new df
    data_new = data_c[1:(nrow(data_c)-1), , drop = FALSE]
    rownames(data_new) = 1:nrow(data_new)
    colnames(data_new) = colnames(data_c)
    #Midpoint of values
    for(z in 1:ncol(data_c)){
      data_z = as.numeric(data_c[, z])
      data_mid = data_z[-length(data_z)] + diff(data_z)/2
      data_new[, z] = data_mid
    }
    #Ensuring stays as data frame when one column
    if(ncol(data_new) == 1){
      colnames(data_new)[[1]] = colnames(data)[[col_s]]
    }
    return(data_new)
  }

##### Random forest classifier OOP implementation #####

Node = 
  R6Class(
    classname = "Node", 
    public = list(
      depth = "numeric", 
      left = NULL,
      right = NULL,
      feature = NULL, 
      value = NULL, 
      initialize =
        function(depth = NULL, left = NULL, right = NULL, feature = NULL, value = NULL){
          self$depth = depth
          self$left = left
          self$right = right
          self$feature = feature
          self$value = value
        })
  )

RandomForest = 
  R6Class(
    classname = "RandomForest",
    public = list(
      min_samples = "numeric", 
      max_depth = "numeric",
      nboot = "numeric",
      ntree = "numeric",
      mode = "character",
      df = "data.frame",
      subspace = "character",
      root = NULL,
      initialize = 
        function(min_samples, max_depth = NULL, nboot = NULL, ntree, mode, df, root = NULL, subspace){
          self$min_samples = min_samples
          self$max_depth = max_depth
          self$mode = mode
          self$df = df
          self$root = root
          self$ntree = ntree
          self$subspace = subspace
          self$nboot = nboot

        },
      get_bootstrap = function(train_df, boot_samp){
        #If bootstrap samples is not provided, randomize
        if(is.null(boot_samp)){
          boot_samp = sample(x = 1:nrow(train_df), size = 1)
        } else {
          boot_samp = boot_samp
        }
        #Sample indices with replacement; replace = T, creates decimal indices as row names cannot be same
        indices = sample(x = 1:nrow(train_df), size = boot_samp, replace = TRUE)
        #Return bootstrapped data
        boot_df = train_df[indices, ]
        
        
        return(boot_df)
        },
      get_entropy =
        function(x){
          if(length(x) == 0) return(0)
          weights = table(x)/length(x)
          info_content = -weights*log2(weights)
          entropy = sum(info_content)
          return(entropy)
        },
      get_gini_impurity =
        function(x){
          #Assume x is a factor with labels
          if(length(x) == 0) return(0)
          weights = table(x)/length(x)
          weights_squared = weights^2
          sum_of_squares = sum(weights_squared)
          gini = 1 - sum_of_squares
          return(gini)
        },
      get_information_gain =
        function(parent, l_child, r_child, mode = self$mode){
          #Get weights in each child
          weight_l = nrow(l_child)/nrow(parent)
          weight_r = nrow(r_child)/nrow(parent)
          #Choose mode
          if(mode == "gini"){
            gain = self$get_gini_impurity(parent[, ncol(parent)]) - (weight_l*self$get_gini_impurity(l_child[, ncol(l_child)]) + weight_r*self$get_gini_impurity(r_child[, ncol(r_child)]))
          } else {
            gain = self$get_entropy(as.character(parent[, ncol(parent)])) - (weight_l*self$get_entropy(as.character(l_child[, ncol(l_child)])) + weight_r*self$get_entropy(as.character(r_child[, ncol(r_child)])))
          }
        },
      check_purity = 
        function(data){
          #Get unique labels
          labels = length(unique(data[, ncol(data)]))
          #Check if there is only one
          ifelse(labels == 1, return(TRUE), return(FALSE))
        },
      classify_data = 
        function(data){
          #Get labels
          get_labels = data[, ncol(data)]
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
        },
      split_data =
        function(data, split_column, split_value){
          split_c = data[[split_column]]
          #Filter the data into above and below
          data_below = data[split_c <= split_value, ]
          data_above = data[split_c > split_value, ]
          return(list(data_above, data_below))
        },
      get_potential_splits =
        function(data){
          #Remove target column
          data_c = data
          col_n = ncol(data_c) - 1
          #Subspacing
          if(self$subspace == TRUE){
            col_s = sample(x = 1:col_n, size = sample(x = 1:col_n, size = 1), replace = FALSE)
          } else {
            col_s = 1:col_n
          }
          data_c = as.data.frame(data[,col_s])
          #Ensuring stays as data frame when one column
          if(ncol(data_c) == 1){
            colnames(data_c)[[1]] = colnames(data)[[col_s]]
          }
          return(data_c)
        },
      determine_best_split =
        function(data, potential_splits, mode = self$mode){
          #Initialize information, feature, feature val
          running_gain = -Inf
          best_split_value = -Inf
          best_split_column = ""
          #Find best entropy over potential splits
          for(j in 1:ncol(potential_splits)){
            for(i in unique(potential_splits[, j])){
              mask_val = i 
              mask_col = j
              splits = self$split_data(data = data, split_column = mask_col, split_value = mask_val)
              relative_gain = self$get_information_gain(parent = data, r_child = splits[[1]], l_child = splits[[2]], mode = mode)
              if(relative_gain > running_gain){
                running_gain = relative_gain
                best_split_value = mask_val
                best_split_column = colnames(potential_splits)[j]
              } else {
                next(i)
              }
            }
          }
          return(list(best_split_column, best_split_value))
        },
      build_tree = 
        function(df = self$df, curr_depth = 0){
          data = df
          #Split until stopping condition are met
          if(!any(nrow(data) < self$min_samples, 
                  curr_depth == self$max_depth)){
            #Keep splitting
            #Get potential and best splits
            potential_splits = self$get_potential_splits(data)
            best_split = self$determine_best_split(data, potential_splits)
            #Stop if a dead split is made (temp)
            if(best_split[[2]] == -Inf){
              leaf.value = self$classify_data(data)
              return(Node$new(value = leaf.value, depth = curr_depth))
            }
            #Record best split value and feature
            split_column = best_split[[1]]
            split_value = best_split[[2]]
            #Split by best combo and assign
            data_split = self$split_data(data, split_column, split_value)
            data_above = data_split[[1]]
            data_below = data_split[[2]]
            #Recursion occurs here
            left_subtree = self$build_tree(df = data_below, curr_depth = curr_depth + 1)
            right_subtree = self$build_tree(df = data_above, curr_depth = curr_depth + 1)
            #Return decision node
            return(Node$new(depth = curr_depth, left = left_subtree, right = right_subtree, feature = split_column, value = split_value))
          } else {
            #Stop splitting
            #Compute leaf node
            leaf.value = self$classify_data(data)
            return(Node$new(value = leaf.value, depth = curr_depth))
          }
        }, 
      print_tree =
        function(tree = self$build_tree(self$df)){
          #Print decision node
          if(!is.null(tree$feature)){
            cat(tree$depth, paste0(paste(rep("\t", tree$depth), collapse = ""), tree$depth), tree$feature, "<=", tree$value, "\n")}
          #Check for nullity of left and right; leaf
          if(!any(is.null(tree$left), is.null(tree$right))){
            self$print_tree(tree = tree$left)
            self$print_tree(tree = tree$right)
          } else {
            cat(paste0(tree$depth, paste(rep("\t", tree$depth), collapse = ""), tree$depth), "predict:", tree$value, "\n")
          }
        },
      fit =
        function(){
          #Build the tree once for predictions only
          self$root = self$build_tree()
        },
      make_prediction = 
        function(y, tree = self$root){
          if(class(tree$value) == "character") return(tree$value)
          partition.val = tree$value
          feature.num = which(names(self$df) == tree$feature)
          if(y[[feature.num]] < partition.val){
            self$make_prediction(y = y, tree = tree$left) 
          } else {
            self$make_prediction(y = y, tree = tree$right)
          }
        }, 
      get_predictions =
        function(Y){
          predict.dt = double(length = nrow(Y))
          for(i in 1:nrow(Y)){
            row.val = as.numeric(Y[i, 1:(ncol(Y)-1)])
            predict.dt[i] = self$make_prediction(y = row.val)
            next(i)
          }
          return(predict.dt)
        },
      build_forest = 
        function(){
          #Initialize trees
          r_tree = list()
          #Build trees according to params
          for(t in 1:self$ntree){
            #If user does not define depth of forest random assign
            if(is.null(self$max_depth)){
              self$max_depth = sample(x = 1:15, size = 1, replace = F)
            }
            r_tree[[t]] = self$build_tree(df = self$get_bootstrap(train_df = self$df, boot_samp = self$nboot))
          }
          return(r_tree)
        },
      print_forest =
        function(){
          for(t in 1:self$ntree){
            cat("Tree", t, "\n\n")
            self$print_tree(tree = self$root[[t]])
            cat("\n\n")
            next(t)               
          }
        },
      forest_fit =
        function(){
          #Build the forest once for predictions and printing only
          #Since root is shared you can only use a single tree, or a whole forest one at a time
          #Fit must also be used earlier in forest
          self$root = self$build_forest()
        }, 
      forest_predict =
        function(forest = self$root, y){
          #Get prediction from each tree in forest
          all_pred = c()
         for(t in 1:self$ntree){
           all_pred[t] = self$make_prediction(y = y, tree = forest[[t]])
           next(t)
         }
          #Find dominant prediction
          f_freq = as.data.frame(table(all_pred))
          f_freq = f_freq[order(f_freq$Freq), ]
          f_max = which(f_freq$Freq == max(f_freq$Freq))
          #Tie breaking label
          if(length(f_max) == 1){
            return(as.character(f_freq[f_max, 1]))
          } else {
            f_max = sample(f_max, size = 1)
            return(as.character(f_freq[f_max, 1]))
          }
        },
      get_forest_prediction = 
        function(Y){
          predict.dt = double(length = nrow(Y))
          for(i in 1:nrow(Y)){
            row.val = as.numeric(Y[i, 1:(ncol(Y)-1)])
            predict.dt[i] = self$forest_predict(y = row.val)
            next(i)
          }
          return(predict.dt)          
        }
      
      
    ))

##### Random forest regressor OOP implementation #####

Node = 
  R6Class(
    classname = "Node", 
    public = list(
      depth = "numeric", 
      left = NULL,
      right = NULL,
      feature = NULL, 
      value = NULL, 
      initialize =
        function(depth = NULL, left = NULL, right = NULL, feature = NULL, value = NULL){
          self$depth = depth
          self$left = left
          self$right = right
          self$feature = feature
          self$value = value
        })
  )

RandomForest = 
  R6Class(
    classname = "RandomForest",
    public = list(
      min_samples = "numeric", 
      max_depth = "numeric",
      nboot = "numeric",
      ntree = "numeric",
      df = "data.frame",
      subspace = "character",
      root = NULL,
      treedf = NULL,
      initialize = 
        function(min_samples, max_depth = NULL, nboot = NULL, ntree, df, root = NULL, subspace){
          self$min_samples = min_samples
          self$max_depth = max_depth
          self$df = df
          self$root = root
          self$ntree = ntree
          self$subspace = subspace
          self$nboot = nboot
        }, 
      get_leaf_value = 
        function(data){
          mean.leaf.val = mean(data[, ncol(data)])
        },
      get_variance =
        function(x){
          var.get = var(x)
          return(var.get)
        },
      get_bootstrap_subspace = 
        function(train_df, boot_samp){
          #If bootstrap samples is not provided, randomize
          if(is.null(boot_samp)){
            boot_samp = sample(x = 1:nrow(train_df), size = 1)
          } else {
            boot_samp = boot_samp
          }
          #Sample indices with replacement; replace = T, creates decimal indices as row names cannot be same
          indices = sample(x = 1:nrow(train_df), size = boot_samp, replace = TRUE)
          #Get bootsrapped data
          boot_df = train_df[indices, ]
          #Subspacing
          if(self$subspace == TRUE){
            col_n = ncol(boot_df)-1
            col_s = sample(x = 1:col_n, size = sample(x = 1:col_n, size = 1), replace = FALSE)
            col_final = append(col_s, ncol(boot_df))
            return(boot_df[,col_final])
          } else {
            return(boot_df)
          }
        },
      get_entropy =
        function(x){
          if(length(x) == 0) return(0)
          weights = table(x)/length(x)
          info_content = -weights*log2(weights)
          entropy = sum(info_content)
          return(entropy)
        },
      get_gini_impurity =
        function(x){
          #Assume x is a factor with labels
          if(length(x) == 0) return(0)
          weights = table(x)/length(x)
          weights_squared = weights^2
          sum_of_squares = sum(weights_squared)
          gini = 1 - sum_of_squares
          return(gini)
        },
      get_information_gain =
        function(parent, l_child, r_child){
          #Get weights in each child
          weight_l = nrow(l_child)/nrow(parent)
          weight_r = nrow(r_child)/nrow(parent)
          #Calculate gain
          gain = self$get_variance(parent[, ncol(parent)]) - (weight_l*self$get_variance(l_child[, ncol(l_child)]) + weight_r*self$get_variance(r_child[, ncol(r_child)]))
          return(gain)
        },
      split_data =
        function(data, split_column, split_value){
          split_c = data[[split_column]]
          #Filter the data into above and below
          data_below = data[split_c <= split_value, ]
          data_above = data[split_c > split_value, ]
          return(list(data_above, data_below))
        },
      get_potential_splits =
        function(data){
          #Remove target column
          data_c = data
          col_n = ncol(data_c) - 1
          col_s = 1:col_n
          data_c = as.data.frame(data[,col_s])
          #Sorting
          for(i in 1:ncol(data_c)){
            data_i = sort(data_c[, i])
            data_c[, i] = data_i
          }
          #Initialize new df
          data_new = data_c[1:(nrow(data_c)-1), , drop = FALSE]
          rownames(data_new) = 1:nrow(data_new)
          colnames(data_new) = colnames(data_c)
          #Midpoint of values
          for(z in 1:ncol(data_c)){
            data_z = as.numeric(data_c[, z])
            data_mid = data_z[-length(data_z)] + diff(data_z)/2
            data_new[, z] = data_mid
          }
          #Ensuring stays as data frame when one column
          if(ncol(data_new) == 1){
            colnames(data_new)[[1]] = colnames(data)[[col_s]]
          }
          return(data_new)
        },
      determine_best_split =
        function(data, potential_splits){
          #Initialize information, feature, feature val
          running_gain = -Inf
          best_split_value = -Inf
          best_split_column = ""
          #Find best entropy over potential splits
          for(j in 1:ncol(potential_splits)){
            for(i in unique(potential_splits[, j])){
              mask_val = i 
              mask_col = j
              splits = self$split_data(data = data, split_column = mask_col, split_value = mask_val)
              relative_gain = self$get_information_gain(parent = data, r_child = splits[[1]], l_child = splits[[2]])
              if(is.na(relative_gain)){
                next(i)
              }
              if(relative_gain > running_gain){
                running_gain = relative_gain
                best_split_value = mask_val
                best_split_column = colnames(potential_splits)[j]
              } else {
                next(i)
              }
            }
          }
          return(list(best_split_column, best_split_value))
        },
      build_tree = 
        function(df = self$df, curr_depth = 0){
          data = df
          #Split until stopping condition are met
          if(!any(nrow(data) < self$min_samples, 
                  curr_depth == self$max_depth)){
            #Keep splitting
            #Get potential and best splits
            potential_splits = self$get_potential_splits(data)
            best_split = self$determine_best_split(data, potential_splits)
            #Stop if a dead split is made (temp)
            if(best_split[[2]] == -Inf){
              leaf.value = self$get_leaf_value(data)
              return(Node$new(value = leaf.value, depth = curr_depth))
            }
            #Record best split value and feature
            split_column = best_split[[1]]
            split_value = best_split[[2]]
            #Split by best combo and assign
            data_split = self$split_data(data, split_column, split_value)
            data_above = data_split[[1]]
            data_below = data_split[[2]]
            #Recursion occurs here
            left_subtree = self$build_tree(df = data_below, curr_depth = curr_depth + 1)
            right_subtree = self$build_tree(df = data_above, curr_depth = curr_depth + 1)
            #Return decision node
            return(Node$new(depth = curr_depth, left = left_subtree, right = right_subtree, feature = split_column, value = split_value))
          } else {
            #Stop splitting
            #Compute leaf node
            leaf.value = self$get_leaf_value(data)
            return(Node$new(value = leaf.value, depth = curr_depth))
          }
        }, 
      print_tree =
        function(tree = self$build_tree(self$df)){
          #Print decision node
          if(!is.null(tree$feature)){
            cat(tree$depth, paste0(paste(rep("\t", tree$depth), collapse = ""), tree$depth), tree$feature, "<=", tree$value, "\n")}
          #Check for nullity of left and right; leaf
          if(!any(is.null(tree$left), is.null(tree$right))){
            self$print_tree(tree = tree$left)
            self$print_tree(tree = tree$right)
          } else {
            cat(paste0(tree$depth, paste(rep("\t", tree$depth), collapse = ""), tree$depth), "predict:", tree$value, "\n")
          }
        },
      fit =
        function(){
          #Build the tree once for predictions only
          self$root = self$build_tree()
        },
      make_prediction = 
        function(y, tree = self$root){
          if(all(is.null(tree$left), is.null(tree$right))) return(tree$value)
          partition.val = tree$value
          feature.num = which(names(self$df) == tree$feature)
          if(y[[feature.num]] < partition.val){
            self$make_prediction(y = y, tree = tree$left) 
          } else {
            self$make_prediction(y = y, tree = tree$right)
          }
        }, 
      get_predictions =
        function(Y){
          predict.dt = double(length = nrow(Y))
          for(i in 1:nrow(Y)){
            row.val = as.numeric(Y[i, 1:(ncol(Y)-1)])
            predict.dt[i] = self$make_prediction(y = row.val)
            next(i)
          }
          return(predict.dt)
        },
      build_forest = 
        function(){
          #Initialize trees and store tree dfs
          r_tree = list()
          p_tree = list()
          #Build trees according to params
          for(t in 1:self$ntree){
            #If user does not define depth of forest random assign
            if(is.null(self$max_depth)){
              self$max_depth = sample(x = 1:15, size = 1, replace = F)
            }
            tree_df = self$get_bootstrap_subspace(train_df = self$df, boot_samp = self$nboot)
            r_tree[[t]] = self$build_tree(df = tree_df)
          }
          return(r_tree)
        },
      print_forest =
        function(){
          for(t in 1:self$ntree){
            cat("Tree", t, "\n\n")
            self$print_tree(tree = self$root[[t]])
            cat("\n\n")
            next(t)               
          }
        },
      forest_fit =
        function(){
          #Build the forest once for predictions and printing only
          self$root = self$build_forest()
        }, 
      forest_predict =
        function(forest = self$root, y){
          #Get prediction from each tree in forest
          all_pred = c()
          for(t in 1:self$ntree){
            all_pred[t] = self$make_prediction(y = y, tree = forest[[t]])
            next(t)
          }
          #Average the prediction from each tree
          average.tree.val = mean(all_pred)
          return(average.tree.val)
        },
      get_forest_prediction = 
        function(Y){
          predict.dt = double(length = nrow(Y))
          for(i in 1:nrow(Y)){
            row.val = as.numeric(Y[i, 1:(ncol(Y)-1)])
            predict.dt[i] = self$forest_predict(y = row.val)
            next(i)
          }
          return(predict.dt)          
        }
      
      
    ))

##### Testing the random forest classifier against a single tree #####

rttsplit = train_test_split(data = iris, test = 30)
rtrain = subset(rttsplit, rttsplit$my.folds == "train")[, -6]
rtest = subset(rttsplit, rttsplit$my.folds == "test")[, -6]

#Different parameter forests
rforest = RandomForest$new(min_samples = 10, max_depth = 5, nboot = 50 ,ntree = 100, mode = "entropy", df = rtrain, subspace = TRUE)
rforest = RandomForest$new(min_samples = 5, ntree = 50, mode = "gini", df = rtrain, subspace = TRUE)
rforest = RandomForest$new(min_samples = 5, ntree = 50, max_depth = 6, nboot = 40, mode = "entropy", df = rtrain, subspace = FALSE)
rforest = RandomForest$new(min_samples = 5, ntree = 1000, max_depth = 2, mode = "entropy", df = rtrain, subspace = TRUE)
rforest = RandomForest$new(min_samples = 5, ntree = 100, mode = "gini", df = rtrain, subspace = TRUE)

#For a single tree, all relevant parameters should be defined (at least one of the stopping conditions)

rforest$print_tree()
rforest$fit()
rforest$make_prediction(y = c(1,2,3,4))
rpredictt = rforest$get_predictions(Y = rtest) == rtest$Species
print((length(which(rpredictt == TRUE)))/(length(rpredictt)))
rforest$get_predictions(Y = rtest) == rtest$Species

#For a forest user does not need to define max_depth or nboot; they can be randomly assigned for each tree

rforest$forest_fit()
rforest$print_forest()
rforest$forest_predict(y = c(1,2,3,4))
rpredictf = rforest$get_forest_prediction(Y = rtest) == rtest$Species
print((length(which(rpredictf == TRUE)))/(length(rpredictf)))

##### Testing the random forest regressor against a single tree #####

rttsplit = train_test_split(data = iris[, c(1:4)], test = )
rtrain = subset(rttsplit, rttsplit$my.folds == "train")[, -5]
rtest = subset(rttsplit, rttsplit$my.folds == "test")[, -5]

#Different parameter forests
rforest = RandomForest$new(min_samples = 15, max_depth = 5, nboot = 100 ,ntree = 1000, df = rtrain, subspace = TRUE)
rforest = RandomForest$new(min_samples = 5, ntree = 500, df = rtrain, subspace = TRUE)
rforest = RandomForest$new(min_samples = 5, ntree = 1000, max_depth = 5, df = rtrain, subspace = TRUE)
rforest = RandomForest$new(min_samples = 5, ntree = 2000, nboot = 75, df = rtrain, subspace = TRUE)
rforest = RandomForest$new(min_samples = 5, ntree = 100, df = rtrain, subspace = TRUE)

#For a single tree, all relevant parameters should be defined (at least one of the stopping conditions)

rforest$print_tree()
rforest$fit()
iris_predict = rforest$get_predictions(Y = rtest)
print(iris_predict)

#For a forest user does not need to define max_depth or nboot; they can be randomly assigned for each tree

rforest$forest_fit()
rforest$print_forest()
rforest$forest_predict(y = c(1,2,3))
iris_predict = rforest$get_forest_prediction(Y = rtest)
print(iris_predict)

(1/length(iris_predict))*sum((rtest$Petal.Width-iris_predict)^2) #mse
(1/length(iris_predict))*sum((rtest$Petal.Width-mean(rtrain$Petal.Width))^2) #mse of bad model

(1/length(iris_predict))*sum(abs(rtest$Petal.Width-iris_predict)) #mae
(1/length(iris_predict))*sum(abs(rtest$Petal.Width-mean(rtrain$Petal.Width))) #mae of bad model

##### Archive #####

forest_predict =
  function(forest = self$root, y){
    all_pred = c()
    for(t in 1:self$ntree){
      y_obs = y
      all_pred[t] = self$make_prediction(y = y_obs, tree = forest[[t]])
      self$forest_features = NULL
      next(t)
    } 
    return(all_pred)
  }

get_tree_features = 
  function(tree){
    if(is.null(tree$feature)){
      
    } else {
      self$forest_features = append(self$forest_features, tree$feature)
      self$forest_features = unique(self$forest_features)
      self$get_tree_features(tree = tree$left)
      self$get_tree_features(tree = tree$right)            
    }
  }
