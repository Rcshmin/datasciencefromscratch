#Decision tree regression

#Import libraries

library(tidyverse)
library(R6)
library(showtext)
library(pilot)

##### Defining the problem #####

font_add_google("Montserrat", "montsr")
showtext_auto()

ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, size = Petal.Length, color = Petal.Length)) +
  geom_point() +
  labs(title = "How can we use sepal length and sepal width to predict petal width", 
       subtitle = "How can we partition this data into its classes?") +
  theme_classic() +
  theme(text=element_text(family="montsr"), 
        plot.title = element_text(size=32),
        plot.subtitle = element_text(size=22),
        axis.title = element_text(size=22),
        axis.text = element_text(size=16),
        legend.text = element_text(size=16), 
        legend.title = element_text(size=22)) +
  scale_colour_gradient(low="#FFBD33", high="074B19") +
  guides(col = "none")

##### New helper functions to deal with numeric target variable #####

get_variance <- function(x){
  if(length(x) == 1) return(Inf)
  var.get = var(x)
  return(var.get)
}

get_leaf_value = 
  function(data){
    mean.leaf.val = mean(data[, ncol(data)])
  }

##### OOP implementation #####

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



Tree = 
  R6Class(
    classname = "Tree",
    public = list(
      min_samples = "numeric", 
      max_depth = "numeric",
      df = "data.frame",
      root = NULL,
      initialize = 
        function(min_samples, max_depth, df, root = NULL){
          self$min_samples = min_samples
          self$max_depth = max_depth
          self$df = df
          self$root = root
        },
      get_leaf_value = 
        function(data){
        mean.leaf.val = mean(data[, ncol(data)])
        },
      get_variance =
        function(x){
          if(any(length(x) == 1, length(x) == 0)) return(Inf)
          var.get = var(x)
          return(var.get)
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
          #Ensure one feature split possible
          potential_splits = as.data.frame(dat)
          if(ncol(potential_splits) == 1){
            colnames(potential_splits)[[1]] = colnames(data)[[1]]
          }
          return(potential_splits)
        },
      determine_best_split =
        function(data, potential_splits){
          #Initialize information, feature, feature val
          running_gain = -Inf
          best_split_value = 0
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
            if(best_split[[2]] == 0){
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
        }
      
      
    ))

##### Testing the OOP Implementation #####

dtree = Tree$new(min_samples = 5, max_depth = 3, df = iris[,c(1,2)])
dtree_store = dtree$build_tree()
dtree_build = dtree$print_tree()

set.seed(2319820)
iris_tt = train_test_split(data = iris[,c(1,2,3)], test = 15)
iris_train = subset(iris_tt, iris_tt$my.folds == "train")[, -4]
iris_test = subset(iris_tt, iris_tt$my.folds == "test")[, -4]
iris_rtree = Tree$new(min_samples = 5, max_depth = 6, df = iris_train)
iris_rbuild = iris_rtree$build_tree()
iris_rtree$print_tree()
iris_rtree$fit()
iris_predict = iris_rtree$get_predictions(Y = iris_test) 
mean(abs((iris_predict - iris_test$Petal.Length)/iris_predict))*100

##### Issues #####

#issue, sometimes a split may predict Na.N; issue might be minimum samples

iris1 = iris_train %>% 
  filter(Sepal.Length > 5.4) %>% 
  filter(Sepal.Length <= 6) %>% 
  filter(Sepal.Width > 3.6)

mean(iris1$Petal.Length)

for(j in 1:1){
  print(j)
}

#issue, did not work with one feature predictor; fixed by editing potential splits

iris_chegg = data.frame(matrix(ncol = 5))
colnames(iris_chegg) = names(iris)
colnames(iris_chegg)[[1]] = "Sepal.Chilly"

ncol(as.data.frame(get_potential_splits(iris[,c(1,2)])))

iris_chheg = get_potential_splits(iris[,c(1,2,3)])

determine_best_split(data = iris[,c(1,2)], potential_splits = get_potential_splits(iris[,c(1,2)]))

#solved by adding an if statement in method for build tree, might revert it later if issue persists

##### Helpers without $self for troubleshooting #####

determine_best_split =
  function(data, potential_splits){
    #Initialize information, feature, feature val
    running_gain = -Inf
    best_split_value = 0
    best_split_column = ""
    #Find best entropy over potential splits
    for(j in 1:ncol(potential_splits)){
      for(i in unique(potential_splits[, j])){
        mask_val = i 
        mask_col = j
        splits = split_data(data = data, split_column = mask_col, split_value = mask_val)
        relative_gain = get_information_gain(parent = data, r_child = splits[[1]], l_child = splits[[2]])
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
  }

get_information_gain =
  function(parent, l_child, r_child){
    #Get weights in each child
    weight_l = nrow(l_child)/nrow(parent)
    weight_r = nrow(r_child)/nrow(parent)
    #Calculate gain
    gain = get_variance(parent[, ncol(parent)]) - (weight_l*get_variance(l_child[, ncol(l_child)]) + get_variance(r_child[, ncol(r_child)]))
    return(gain)
  }

split_data =
  function(data, split_column, split_value){
    split_c = data[[split_column]]
    #Filter the data into above and below
    data_below = data[split_c <= split_value, ]
    data_above = data[split_c > split_value, ]
    return(list(data_above, data_below))
  }

get_potential_splits =
  function(data){
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
    potential_splits = as.data.frame(dat)
    if(ncol(potential_splits) == 1){
      colnames(potential_splits)[[1]] = colnames(data)[[1]]
    }
    return(potential_splits)
  }

get_variance =
  function(x){
    if(any(length(x) == 1, length(x) == 0)) return(Inf)
    var.get = var(x)
    return(var.get)
  }
  