
library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)				        # Popular decision tree algorithm
library(rattle)					# Fancy tree plot
library(rpart.plot)				# Enhanced tree plots
library(RColorBrewer)				# Color selection for fancy tree plot
library(party)					# Alternative decision tree algorithm
library(partykit)				# Convert rpart object to BinaryTree
library(caret)
library(stringr)
library(data.table)
library(dplyr)
library(randomForest)
library(memisc)
library(plotly)

subset_columns <- function(data,targetvariable,cols_in_rf){  #target variable is inputted as column vector

  newdata <- as.data.frame(data[[targetvariable]])  ## we intialize the new dataset that this iteration of the random forest will run on (decision tree)

  colnames(newdata)<- c("target")




  for (i in 1:cols_in_rf){

    target_num <- grep(targetvariable, colnames(data))

    ncols <- ncol(data)

    random_col_num <- round(runif(1, min=1, max=ncols))

    if(random_col_num == target_num)
    {

    }
    else
    {
      newdata[[colnames(data[random_col_num])]] <- data[,c(colnames(data[random_col_num]))] # this grabs the random column vector and assigns it to the new subsetted dataset

      data[,c(colnames(data[random_col_num]))] <- NULL #removes the chosen column so that it is not chosen again for the particular tree
    }

  }

  return(newdata)
}


#test <- subset_columns(data,data$Action,10)


subset_rows <- function(data,targetvariable,percent_of_rows){

  train.index <- createDataPartition(data[[targetvariable]], p = percent_of_rows, list = FALSE)
  data_out <- data[ train.index,]


  return(data_out)
}

#test <- subset_rows(data,data$Action,.5)






RF_with_Nulls <- function(data,data_to_score,targetvariable,percent_of_rows,cols_in_rf,iterations,complexity_parameter,minbucket,print_every_ithtree){

  for (i in 1:iterations){
    training_data <- subset_rows(data, targetvariable,percent_of_rows)

    training_data2 <- subset_columns(training_data,targetvariable,cols_in_rf)



    form <- as.formula(target~ .)

    assign(paste("model_", i, sep = ""), rpart(form,data=training_data2,control=rpart.control(cp=complexity_parameter,minbucket = minbucket)))

    data_to_score[[paste("prediction_", i, sep = "")]] <- predict(get(paste0("model_", i)), newdata=data_to_score)

    if(i %% print_every_ithtree == 0)
    {
      fancyRpartPlot(rpart(form,data=training_data2,control=rpart.control(cp=complexity_parameter,minbucket = minbucket)),palettes=("Reds"))
    }
  }

  ### creating a list of models
  models <<- vector("list", iterations)
  i <- 1
  while( i <= iterations) {
    current_model <- get(paste0("model_",i))
    models[[i]] <<- current_model$variable.importance
    i <- i + 1
  }
  ##################################
  
  data_to_score[["prediction_overall"]] <- 0

  for (i in 1:iterations){
    data_to_score[["prediction_overall"]]<-data_to_score[["prediction_overall"]] + data_to_score[[paste("prediction_", i, sep = "")]]

    data_to_score[[paste("prediction_", i, sep = "")]]<- NULL

  }

  data_to_score[["prediction_overall"]] <- data_to_score[["prediction_overall"]]/iterations


  return(data_to_score)

}


