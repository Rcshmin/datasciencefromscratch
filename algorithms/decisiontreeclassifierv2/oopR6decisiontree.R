#Decision tree v2 attempt

library(R6)
library(tidyverse)

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
      mode = "character",
      df = "data.frame",
      root = NULL,
      initialize = 
        function(min_samples, max_depth, mode, df, root = NULL){
          self$min_samples = min_samples
          self$max_depth = max_depth
          self$mode = mode
          self$df = df
          self$root = root
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
        },
      determine_best_split =
        function(data, potential_splits, mode = self$mode){
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
          if(!any(self$check_purity(data), 
                  nrow(data) < self$min_samples, 
                  curr_depth == self$max_depth)){
            #Keep splitting
            #Get potential and best splits
            potential_splits = self$get_potential_splits(data)
            best_split = self$determine_best_split(data, potential_splits, mode = self$mode)
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
        }
      
      
    ))

##### Testing the OOP implementation #####

dtree = Tree$new(min_samples = 10, max_depth = 5, mode = "gini", df = salmon_fish)
dtree_p = dtree$print_tree()
dtree_b = dtree$build_tree()
dtree$fit()
dtree$make_prediction(y = c(4.5, 7.5))

otree = Tree$new(min_samples = 10, max_depth = 5, mode = "entropy", df = iris[,c(4,5)])
otree_p = otree$print_tree()
otree_b = otree$build_tree()
otree$fit()
otree$make_prediction(y = c(4.5))

dtree = Tree$new(min_samples = 10, max_depth = 5, mode = "entropy", df = salmon_fish)
dtree_p = dtree$print_tree()
dtree_b = dtree$build_tree()
dtree$fit()
dtree$make_prediction(y = c(4.5))

salmon_fish_tt = train_test_split(data = salmon_fish, test = 200)
salmon_fish_train = subset(salmon_fish_tt, salmon_fish_tt$my.folds == "train")[, -4]
salmon_fish_test = subset(salmon_fish_tt, salmon_fish_tt$my.folds == "test")[, -4]
dtree = Tree$new(min_samples = 10, max_depth = 10, mode = "entropy", df = salmon_fish_train)
dtree$fit()
salmon_fish_test$type == dtree$get_predictions(Y = salmon_fish_test)

stree = Tree$new(min_samples = 5, max_depth = 5, mode = "gini", df = iris[, c(1,4,5)])
stree_p = stree$print_tree()
stree_b = stree$build_tree()
stree$fit()
stree$make_prediction(y = c(7.5, 2))

iris_tt = train_test_split(data = iris, test = 15)
iris_train = subset(iris_tt, iris_tt$my.folds == "train")[, -6]
iris_test = subset(iris_tt, iris_tt$my.folds == "test")[, -6]
stree = Tree$new(min_samples = 5, max_depth = 6, mode = "entropy", df = iris_train)
stree$fit()
iris_test$Species == stree$get_predictions(Y = iris_test) 

##### Graphing the splits manually #####

ggplot(data = salmon_fish, aes(x = length, y = weight, color = type)) +
  geom_point() +
  geom_vline(xintercept = 2.996) +
  geom_vline(xintercept = 6.978) +
  geom_vline(xintercept = 3.398) +
  geom_vline(xintercept = 4.998) + 
  geom_vline(xintercept = 6.071) +
  geom_hline(yintercept = 4.006) +
  geom_hline(yintercept = 6.793) +
  geom_hline(yintercept = 6.948) +
  geom_hline(yintercept = 6.832) +
  labs(title = "Classifying fish by weight and length", 
       subtitle = "How can we partition this data into its classes?") +
  theme_classic() +
  theme(text=element_text(family="montsr"), 
        plot.title = element_text(size=32),
        plot.subtitle = element_text(size=22),
        axis.title = element_text(size=22),
        axis.text = element_text(size=16),
        legend.text = element_text(size=16), 
        legend.title = element_text(size=22)) +
  scale_color_pilot()


ggplot(data = iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) +
  geom_point() +
  geom_vline(xintercept = 0.8) +
  geom_vline(xintercept = 1.7) +
  geom_vline(xintercept = 1.3) +
  geom_hline(yintercept = 5.9) +
  geom_hline(yintercept = 5.05) +
  geom_hline(yintercept = 7.1)

##### Troubleshooting the methods #####
  
  
  

