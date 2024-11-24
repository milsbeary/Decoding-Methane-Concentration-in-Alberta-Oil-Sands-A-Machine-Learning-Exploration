library(gam)
library(ggplot2)
library(randomForest)
library(tidyverse)

set.seed(129013)
#This function was adapted from: https://github.com/damariszurell/SSDM-JSDM/blob/master/Univariate_variable_importance_blockCV_select07.r
#This function is used to rank the most important predictors 
select07_GLM <- function(pred_names, response_name, data, threshold=thre){
  # Function for calculating AIC - we use univariate GLMs with linear and quadratic terms
  var.imp <- function (predictor, response){
    glm_var <- glm(response ~ predictor)
    AIC(glm_var)
  }         
  # Calculate AIC for all predictor variables
  aic_imp <- apply(data[pred_names], 2, var.imp, response= data[,response_name])
  # Names of sorted variables
  sort_imp <- names(sort(aic_imp))
  # Calculate correlation matrix if not provided in function call
  cor_mat <- cor(data[pred_names], method='spearman')
  # Identifies correlated variable pairs:
  diag(cor_mat)=NA
  pairs <- which(abs(cor_mat)>= threshold, arr.ind=T) 
  # Identify which variables should be excluded
  exclude <- NULL
  for (i in 1:length(sort_imp))
  {
    if ((sort_imp[i] %in% row.names(pairs))&
        ((sort_imp[i] %in% exclude)==F)) {
      cv <- cor_mat[base::setdiff(row.names(cor_mat),exclude),sort_imp[i]]
      cv <- cv[base::setdiff(names(cv),sort_imp[1:i])]
      exclude <- c(exclude,names(which((abs(cv)>=threshold)))) 
    }
  }
  
  # Select set of weakly correlated predictors:
  pred_sel <- sort_imp[!(sort_imp %in% exclude)]
  # Return list with AIC, correlation matrix, and final predictors:
  return(list(AIC=sort(aic_imp), cor_mat=cor_mat, pred_sel=pred_sel))  
}

Stations <- c('Sta',"Stn")  #Codes of all the studied stations
Group_type <- c("nlc") #The group type
Transformations <- "Pure" # Variable transformations used 
models_char <- 'RF' # ML models 

#######CROSS-VALIDATION


#Full prediction using RF (we perdormed it only for two stations Sta and Stn)


Final_predictions <- as.data.frame(matrix(NA, nrow=1, ncol=10))
colnames(Final_predictions) <- c("Model", "Predicted_CH4", "Observed_CH4", "Station", "Year_M_D_H_M", "Residual", "Data", "ScalOrNorm", "Group_type", "Transformation")

for(k in seq_along(Stations)){
  Variable_importance_ <- read.csv(paste0("/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/Data_10_05_2024/Pure/variable_importance.csv" ), sep=";")[-1]
  
  Variable_importance <- Variable_importance_[c(Variable_importance_$Group_type=="nlc" & Variable_importance_$df==Stations[k]),][2:16]
  Variable_importance <- Variable_importance[!is.na(Variable_importance)]
  my_preds <- Variable_importance
  
  my_preds_continuous <- my_preds #setdiff(my_preds, my_preds_categorical)
  
  ####Full model : 
  Data_to_use <- read.csv(paste0("/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/Data_10_05_2024/2019-2022/", Stations[k], "_nlc_2019_2022_norm.csv"), sep=",")[,-c(1)]
  Data_to_use <- Data_to_use[c("CH4..ppm.",my_preds)]
  
  Data_full_ <- read.csv(paste0("/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/Data_10_05_2024/Full/", Stations[k], "_nlc_full_norm.csv"), sep=",")[,-c(1)]
  Data_to_use_full_ <- Data_full_[c("CH4..ppm.",my_preds)]
  Prediction_all_models <- data.frame(row = row.names(Data_to_use_full_), 
                                      preds_RF = numeric(length = nrow(Data_to_use_full_))) 
  RF_MODEL <- randomForest(CH4..ppm.~., data= Data_to_use, ntree = 2000, importance = TRUE)
  
  Prediction_all_models[,2]<-  predict(RF_MODEL, Data_to_use_full_, type='response')
  
  Prediction_all_models$Origina_CH4 <- Data_full_$CH4..ppm.
  Prediction_all_models$Station <- rep(Variable_importance_[c(Variable_importance_$Group_type=="nlc" & Variable_importance_$df==Stations[k]),"df"], nrow(Prediction_all_models))
  
  Final_predictions_ <- as.data.frame(matrix(NA, nrow=nrow(Prediction_all_models), ncol=10))
  colnames(Final_predictions_) <- c("Model", "Predicted_CH4", "Observed_CH4", "Station", "Year_M_D_H_M", "Residual", "Data", "ScalOrNorm", "Group_type", "Transformation")
  #CV_stacked_$Index <- Data_full_$Index
  Final_predictions_$Model <- rep("RF", nrow(Final_predictions_))
  Final_predictions_$Predicted_CH4 <- Prediction_all_models[, 2]
  Final_predictions_$Observed_CH4 <- Prediction_all_models[, 3]
  Final_predictions_$Station <- rep(Variable_importance_[c(Variable_importance_$Group_type=="nlc" & Variable_importance_$df==Stations[k]),"df"], nrow(Final_predictions_))
  Final_predictions_$Year_M_D_H_M <- Data_full_$yyyy.MM.dd.HH.mm 
  Final_predictions_$Residual <- Final_predictions_$Predicted_CH4-Final_predictions_$Observed_CH4
  Final_predictions_$Data <- rep(Variable_importance_[c(Variable_importance_$Group_type=="nlc"& Variable_importance_$df==Stations[k]),"df"], nrow(Final_predictions_))
  Final_predictions_$ScalOrNorm <- rep("nrm", nrow(Final_predictions_))
  Final_predictions_$Group_type <- rep("nlc", nrow(Final_predictions_))
  Final_predictions_$Transformation <- rep("Pure", nrow(Final_predictions_))
  
  Final_predictions <- rbind(Final_predictions, Final_predictions_)
  
  print(Final_predictions_$Data[1])
}  
Final_predictions <- Final_predictions[-1,]
Final_predictions$Year_M_D_H_M <- as.Date(Final_predictions$Year_M_D_H_M )


write.csv(Final_predictions, paste0('/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/Prediction_stations.csv'), row.names = F)
