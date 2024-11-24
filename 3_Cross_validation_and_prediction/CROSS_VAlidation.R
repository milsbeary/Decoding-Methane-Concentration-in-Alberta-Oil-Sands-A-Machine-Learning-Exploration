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

Stations <- c("Anz","Ath","Ber","Buf","Con", "Jan", "Low","Man","Mil","Pat",'Sto','Sta',"Stn")  #Codes of all the studied stations
Group_type <- c("nlc") #The group type
Transformations <- c("Interact", "Everything", "Pure", "Trans")# Variable transformations used 
models_char <- c('GLM', 'RF', 'GAM') # ML models 

table_recap_R_2 <- as.data.frame(matrix(NA, nrow=1, ncol=18)) # Table of the accurancy (R^2)
colnames(table_recap_R_2) <- c("ScalOrNorm", "Group_type", "Accuracy", "Transformation", "Model", Stations_All)
table_recap_RMSE <- as.data.frame(matrix(NA, nrow=1, ncol=18))  # Table of the accurancy (RMSE)
colnames(table_recap_RMSE) <- c("ScalOrNorm", "Group_type", "Accuracy", "Transformation","Model", Stations_All)



Final_predictions <- as.data.frame(matrix(NA, nrow=1, ncol=14))
colnames(Final_predictions) <- c("Model", "Predicted_CH4", "Observed_CH4", "Predicted_average", "rmse_Pred_Obs", "Cor_Pred_Obs", "Land_Cover_majority", "Station", "Year_M_D_H_M", "Residual", "Data", "ScalOrNorm", "Group_type", "Transformation")
CV <- as.data.frame(matrix(NA, nrow=1, ncol=11))
colnames(CV) <- c("Index", "Model", "Predicted_CH4", "Observed_CH4", "Predicted_average","Cor_Pred_Obs", "rmse_Pred_Obs", "Data", "ScalOrNorm", "Group_type", "Transformation"  )


#######CROSS-VALIDATION

for(l in seq_along(Group_type)){
  for (tran in seq_along(Transformations)){
    table_recap_R_2_ <- as.data.frame(matrix(NA, nrow=3, ncol=18))
    colnames(table_recap_R_2_) <- c("ScalOrNorm", "Group_type", "Accuracy", "Transformation", "Model", Stations)
    table_recap_R_2_$ScalOrNorm <- rep("nrm", 3)
    table_recap_R_2_$Group_type <- rep(Group_type[l], 3)
    table_recap_R_2_$Accuracy <- rep("R^2", 3)
    table_recap_R_2_$Transformation <- rep(Transformations[tran], 3)
    table_recap_R_2_$Model <- models_char
    
    
    table_recap_RMSE_ <- as.data.frame(matrix(NA, nrow=3, ncol=18))
    colnames(table_recap_RMSE_) <- c("ScalOrNorm", "Group_type", "Accuracy", "Transformation","Model", "Anz","Ath","Ber","Buf","Con", "Jan", "Low","Man","Mil","Pat",'Sto',  'Sta',"Stn")
    table_recap_RMSE_$ScalOrNorm <- rep("nrm", 3)
    table_recap_RMSE_$Group_type <- rep(Group_type[l], 3)
    table_recap_RMSE_$Accuracy <- rep("rmse", 3)
    table_recap_RMSE_$Transformation <- rep(Transformations[tran], 3)
    table_recap_RMSE_$Model <- models_char
    
    for(k in seq_along(Stations)){
      Variable_importance_ <- read.csv(paste0("/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/Data_10_05_2024/", Transformations[tran], "/variable_importance.csv" ), sep=";")[-1]
      
      Variable_importance <- Variable_importance_[c(Variable_importance_$Group_type==Group_type[l] & Variable_importance_$df==Stations[k]),][2:16]
      Variable_importance <- Variable_importance[!is.na(Variable_importance)]
      
      Data <- read.csv(paste0("/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/Data_10_05_2024/", Transformations[tran],"/top_15/", Stations[k], "_",Group_type[l],  "_top_15.csv" ), sep=",")[,-c(1)]
      Data[Data == "."] <- NA
      Data[Data == "Z"] <- NA
      Data <- na.omit(Data)
      Data$Index <- c(1:nrow(Data))
      Data_stack <- Data
      
      my_preds <- Variable_importance #Predictors
      
      Data$Index <- c(1:nrow(Data))
      
      Data_ <- Data
      
      my_preds_categorical <- c("") #No categorical variables are used 
      
      my_preds_continuous <- my_preds #setdiff(my_preds, my_preds_categorical)
      crossval_all_models <- data.frame(row = row.names(Data_), preds_GLM = numeric(length = nrow(Data_)), 
                                        preds_RF = numeric(length = nrow(Data_)),preds_GAM = numeric(length = nrow(Data_))) 
      Data_to_use <- Data_[c(my_preds, "CH4..ppm.")]
      kfold=5
      ks <- kfold(Data_to_use, k = kfold)
      
      for(s in seq_len(kfold)){
        print(s)
        train <- Data_to_use[ks!=s,]
        test <- Data_to_use[ks==s,]
        #GLM-------------------------------------------
        MODEL_FORM <- paste0('CH4..ppm. ~', paste(paste(my_preds_continuous,collapse = "+"), collapse = "+"))
        
        GLM_MODEL <- step(lm(as.formula (MODEL_FORM), data=train), trace = 0)
        crossval_all_models[which(ks==s),2]<-  predict(GLM_MODEL, test, type='response')
        #RF----------------------------------------------
        RF_MODEL <- randomForest(CH4..ppm.~., data= train, ntree = 2000, importance = TRUE)
        
        crossval_all_models[which(ks==s),3]<-  predict(RF_MODEL, test, type='response')
        #GAM--------------------------------------------
        
        new_data_train <- train[my_preds_continuous]^2
        colnames(new_data_train) <- paste0(colnames(train[my_preds_continuous]), "_exp_2")
        new_data_train <- cbind(train[my_preds_continuous], new_data_train)
        pre_predictor <- c(colnames(new_data_train))
        new_data_train$CH4..ppm. <- train$CH4..ppm.
        
        my_predictors <- pre_predictor
        var_sel <- c()
        var_sel <- select07_GLM(pred_names=my_predictors, response_name="CH4..ppm.", data=new_data_train, threshold=0.6)
        my_predictor<- c( var_sel[["pred_sel"]])
        
        form<-as.formula(paste0("CH4..ppm. ~",paste(paste0(my_predictor, collapse="+"))))
        GAM_MODEL <- gam(formula=form, family='gaussian', data=new_data_train)
        
        new_test <- test^2
        colnames(new_test) <- paste0(colnames(new_test[my_preds_continuous]), "_exp_2")
        new_test <- cbind(test, new_test)
        new_test <- new_test[c("CH4..ppm.",my_predictor)]
        
        crossval_all_models[which(ks==s),4]<-  predict(GAM_MODEL, new_test, type='response')
      }  
      crossval_all_models$Origina_CH4 <- Data_to_use$CH4..ppm.
      crossval_all_models$Average_pred <- colMeans(t(crossval_all_models[,c(2:4)]))
      CV_stacked_stacked <- as.data.frame(matrix(NA, nrow=1, ncol=11))
      colnames(CV_stacked_stacked) <- c("Index", "Model", "Predicted_CH4", "Observed_CH4", "Predicted_average","Cor_Pred_Obs", "rmse_Pred_Obs", "Data", "ScalOrNorm", "Group_type", "Transformation" )
      
      
      for(i in seq_along(models_char)){
        
        CV_stacked_ <- as.data.frame(matrix(NA, nrow=nrow(crossval_all_models), ncol=11))
        colnames(CV_stacked_) <- c("Index", "Model", "Predicted_CH4", "Observed_CH4", "Predicted_average","Cor_Pred_Obs", "rmse_Pred_Obs", "Data", "ScalOrNorm", "Group_type", "Transformation")
        CV_stacked_$Index <- c(1:nrow(CV_stacked_))
        CV_stacked_$Model <- rep(models_char[i], nrow(CV_stacked_))
        CV_stacked_$Observed_CH4 <- crossval_all_models[, 5]
        CV_stacked_$Predicted_CH4 <- crossval_all_models[, (i+1)]
        CV_stacked_$Predicted_average <- crossval_all_models[, 6]
        ml = lm(Predicted_CH4~Observed_CH4, data = CV_stacked_) 
        CV_stacked_$Cor_Pred_Obs <- summary(ml)$r.squared 
        CV_stacked_$rmse_Pred_Obs <- rmse(sim=CV_stacked_$Predicted_CH4, obs=CV_stacked_$Observed_CH4)
        CV_stacked_stacked <- rbind(CV_stacked_stacked, CV_stacked_)
        table_recap_R_2_[i,  which( colnames(table_recap_R_2_)==Stations[k])] <- summary(ml)$r.squared 
        table_recap_RMSE_[i,  which( colnames(table_recap_RMSE_)==Stations[k])] <- rmse(sim=CV_stacked_$Predicted_CH4 , obs=CV_stacked_$Observed_CH4)
        
      }
      CV_stacked <- CV_stacked_stacked[-1,]
      CV_stacked$Data <- rep(Variable_importance_[c(Variable_importance_$Group_type==Group_type[l] & Variable_importance_$df==Stations[k]),"df"], nrow(CV_stacked))
      CV_stacked$ScalOrNorm <- rep("nrm", nrow(CV_stacked))
      CV_stacked$Group_type <- rep(Group_type[l], nrow(CV_stacked))
      CV_stacked$Transformation <- rep(Transformations[tran], nrow(CV_stacked))
      
      
      CV <- rbind(CV, CV_stacked)
      
      print(CV_stacked$Data[1])
    }  
    table_recap_R_2 <- rbind(table_recap_R_2, table_recap_R_2_)
    table_recap_RMSE <- rbind(table_recap_RMSE, table_recap_RMSE_)
    table_recap_ <- rbind(table_recap_R_2_,table_recap_RMSE_)
    table_recap_<- cbind(table_recap_[, c(3:5)],round(table_recap_[,c(6:18)], digits=3))
    write.csv(table_recap_, paste0('/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/tab_recap_Trans', Transformations[tran],'.csv'), row.names = F)
    print(table_recap_)
  }  
}
table_recap_R_2 <- table_recap_R_2[-1,]
table_recap_RMSE <- table_recap_RMSE[-1,]

table_recap <- rbind(table_recap_R_2,table_recap_RMSE)
table_recap<- cbind(table_recap[, c(3:5)],round(table_recap[,c(6:18)], digits=3))
write.csv(table_recap, paste0('/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/tab_recap_nlc_rmse.csv'), row.names = F)

CV <- CV[-1,]

write.csv(CV, paste0('/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/Prediction_CV_rmse.csv'), row.names = F)

#Full prediction using RF (we perdormed it only for two stations Sta and Stn)
Stations <- c('Sta',"Stn")

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

###################
#######Plots#######
###################


df_weekly_Est <- as.data.frame(matrix(NA, nrow=1,ncol=5))
colnames(df_weekly_Est) <- c("week_num", "mean_CH4","year",  "Station", "Type")
my_station <-  c('Sta',"Stn")
for(i in seq_along(my_station)){
  df_ <- Final_predictions[Final_predictions$Data==my_station[i],]
  df_$year <- strftime(df_$Year_M_D_H_M, format = "%Y")
  num_years <- unique(strftime(df_$Year_M_D_H_M, format = "%Y"))
  print(num_years)
  for(j in seq_along(num_years)){
    df <- df_[strftime(df_$Year_M_D_H_M, format = "%Y")==num_years[j],]
    df$week_num <- strftime(df$Year_M_D_H_M, format = "%V")
    df2 <- df %>%
      group_by(week_num) %>%
      summarize(mean_CH4 = mean(Predicted_CH4))
    df2$year <- rep(num_years[j], nrow(df2))
    df2$Station <- rep(my_station[i], nrow(df2))
    df2$Type <- rep("Prediction", nrow(df2))
    df2$week_num  <- as.numeric(df2$week_num)
    df_weekly_Est <- rbind(df_weekly_Est, df2)
  }
}
df_weekly_Est <- df_weekly_Est[-1,]
df_weekly_Obs <- as.data.frame(matrix(NA, nrow=1,ncol=5))
colnames(df_weekly_Obs) <- c("week_num", "mean_CH4","year",  "Station", "Type")
my_station <-  c('Sta',"Stn")
for(i in seq_along(my_station)){
  df_ <- Final_predictions[Final_predictions$Data==my_station[i],]
  df_$year <- strftime(df_$Year_M_D_H_M, format = "%Y")
  num_years <- unique(strftime(df_$Year_M_D_H_M, format = "%Y"))
  print(num_years)
  for(j in seq_along(num_years)){
    df <- df_[strftime(df_$Year_M_D_H_M, format = "%Y")==num_years[j],]
    df$week_num <- strftime(df$Year_M_D_H_M, format = "%V")
    df2 <- df %>%
      group_by(week_num) %>%
      summarize(mean_CH4 = mean(Observed_CH4))
    df2$year <- rep(num_years[j], nrow(df2))
    df2$Station <- rep(my_station[i], nrow(df2))
    df2$Type <- rep("Observation", nrow(df2))
    df2$week_num  <- as.numeric(df2$week_num)
    df_weekly_Obs <- rbind(df_weekly_Obs, df2)
  }
}
df_weekly_Obs <- df_weekly_Obs[-1,]
df_weekly <- rbind(df_weekly_Obs,df_weekly_Est)


df_weekly_sta <- df_weekly[df_weekly$Station=="Sta",]
Prediction_vs_Habitat_plot_TOTAL_Sta <- 
  ggplot(df_weekly_sta,aes(x=week_num , y=mean_CH4, color=Type))+#facet_grid(cols = vars(Cluster))+
  geom_point(size = 2, alpha = 0.8) + # theme(legend.position="none")+
  labs(title = "Methane weekly Predictions vs. Observations" , subtitle = "Stacked-stations with gases",
       #  subtitle = "Density Plot",
       x = "Week", y = "CH4")+facet_grid(cols = vars(year))+ theme_bw()+
  theme(strip.text = element_text(
    size = 20, color = "black"))+
  theme(axis.text.x = element_text(angle = 90, size = 18),  axis.title.x=element_text(size = 18), axis.text.y = element_text(size = 18),  axis.title.y = element_text(size = 18),
        legend.title = element_text( size = 20), legend.text = element_text( size = 20), 
        plot.title = element_text(size=22))+
  theme(plot.subtitle=element_text(size=18, color="black"))
# scale_colour_manual(values = c("Water" = "#2166ac",
#                                 "Tree" =  "#858585",
#                                "Shrub" = "#B2182B",
#                               "Bare ground" = "#D1E5F0",
#                               "Snow"= "#D55E00"))

png(width = 1500, height = 500,paste0("/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/predictions_WEEKLY_sta.png"),res=100)
print(Prediction_vs_Habitat_plot_TOTAL_Sta)
dev.off()

df_weekly_stn <- df_weekly[df_weekly$Station=="Stn",]
Prediction_vs_Habitat_plot_TOTAL_Stn <- 
  ggplot(df_weekly_stn,aes(x=week_num , y=mean_CH4, color=Type))+#facet_grid(cols = vars(Cluster))+
  geom_point(size = 2, alpha = 0.8) + # theme(legend.position="none")+
  labs(title = "Methane weekly Predictions vs. Observations" , subtitle = "Stacked-stations without gases",
       #  subtitle = "Density Plot",
       x = "Week", y = "CH4")+facet_grid(cols = vars(year))+
  theme_bw()+
  theme(strip.text = element_text(
    size = 20, color = "black"))+
  theme(axis.text.x = element_text(angle = 90, size = 18),  axis.title.x=element_text(size = 18), axis.text.y = element_text(size = 18),  axis.title.y = element_text(size = 18),
        legend.title = element_text( size = 20), legend.text = element_text( size = 20), 
        plot.title = element_text(size=22))+
  theme(plot.subtitle=element_text(size=18, color="black"))

# scale_colour_manual(values = c("Water" = "#2166ac",
#                                 "Tree" =  "#858585",
#                                "Shrub" = "#B2182B",
#                               "Bare ground" = "#D1E5F0",
#                               "Snow"= "#D55E00"))

png(width = 1500, height = 500,paste0("/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/predictions_WEEKLY_stn.png"),res=100)
print(Prediction_vs_Habitat_plot_TOTAL_Stn)
dev.off()










formula <- y ~ poly(x, 1, raw = TRUE)
CV_ <- CV [CV$Transformation=="Pure",]
CV_plot <- 
  ggplot(CV_,aes(x=Observed_CH4 , y=Predicted_CH4, label = Model, color=Model))+#facet_grid(cols = vars(Cluster))+
  geom_point(size = 2, alpha = 0.8) +
  # geom_xsidedensity() +
  #geom_ysidedensity() +
  #scale_color_tq() +
  # scale_fill_tq() +
  theme_bw() +
  labs(title = "Cross-validation accuracy (Predictions vs. Observations)" ,
       #  subtitle = "Density Plot",
       x = "Observed CH4", y = "Predicted CH4")+ facet_wrap(~Data)+#+facet_grid(cols = vars(Data),scales= "free" )+
  geom_smooth(method = "lm", formula = formula) +
  # stat_poly_eq(aes(label = after_stat(adj.rr.label)), formula = formula, 
  #              parse = TRUE,label.y = "bottom")+
  stat_smooth(aes(label = Model, color=Model), method = "lm", formula = formula) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula
  ) +  # geom_text(aes(label=ifelse(Dissimilarity<=0.5,as.character(Species),'')),hjust=0,vjust=0)+
  theme(axis.text.x = element_text(angle = 90, size = 18),  axis.title.x=element_text(size = 18), axis.text.y = element_text(size = 18),  axis.title.y = element_text(size = 18),
        legend.title = element_text( size = 18), legend.text = element_text( size = 18), 
        plot.title = element_text(size=22))+
  scale_colour_manual(values = c("GAM" = "#2166ac",
                                 "GLM" =  "#858585",
                                 "RF"= "#D55E00"))

#guides(fill = guide_legend(title="Sorensen dissimilarity"))
#stat_poly_eq(aes(label = paste( after_stat(rr.label), sep = "*\", \"*"))) +
#stat_poly_eq(aes(label = ..eq.label..),
#               formula = formula, parse = TRUE)+

png(width = 1500, height = 1500,paste0("/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/Scaled_data-4/CV.png"),res=100)
print(CV_plot)
dev.off()


# plot residuals
Accuaary_final$x <- c(2.5, 2.5, 2.5)
Accuaary_final$y <- c(0.5, 0.5, 0.5)
Final_predictions_RF <- Final_predictions[Final_predictions$Model=="RF_MODEL",]

Residual_plot <- 
  ggplot(Final_predictions_RF,aes(x=Observed_CH4 , y=Residual, label = Model, color=Model))+#facet_grid(cols = vars(Cluster))+
  geom_point(size = 2, alpha = 0.8) +   facet_wrap(~Data)+ 
  geom_smooth(method = "lm", formula = formula) +
  #stat_poly_eq(aes(label = after_stat(adj.rr.label)), formula = formula, 
  #             parse = TRUE,label.y = "bottom")+
  # stat_poly_eq(aes(label = after_stat(adj.rr.label)), formula = formula, 
  #              parse = TRUE,label.y = "bottom")+
  stat_smooth(aes(color=Model), method = "lm", formula = formula) +
  theme(axis.text.x = element_text(angle = 90, size = 18),  axis.title.x=element_text(size = 18), axis.text.y = element_text(size = 18),  axis.title.y = element_text(size = 18),
        legend.title = element_text( size = 18), legend.text = element_text( size = 18), 
        plot.title = element_text(size=22))+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula, label.y = -5) +
  #geom_text(data = Accuaary_final,
  #          mapping = aes(x = x, y = y, label = Label))+
  # geom_xsidedensity() +
  #geom_ysidedensity() +
  #scale_color_tq() +
  # scale_fill_tq() +
  theme_bw() +  scale_colour_manual(values = c("RF_MODEL"= "#D55E00"))
labs(title = "Residuals vs Observed CH4 (predicted-observed)" ,
     #  subtitle = "Density Plot",
     x = "Observed CH4", y = "Residual")


png(width = 1500, height = 1500,paste0("/Users/ilhem/Dropbox/Mon disque/UAlberta/Air_pollution project/Scaled_data-4/Residual_plot.png"),res=100)
print(Residual_plot)
dev.off()

Final_predictions_RF <- Final_predictions[c(Final_predictions$Model=="RF"& Final_predictions$Data %in% c('Sta',"Stn")),]
Final_predictions_RF$Land_Cover_majority[Final_predictions_RF$Land_Cover_majority == '0'] <- 'Water'
Final_predictions_RF$Land_Cover_majority[Final_predictions_RF$Land_Cover_majority == '1'] <- 'Tree'
Final_predictions_RF$Land_Cover_majority[Final_predictions_RF$Land_Cover_majority == '5'] <- 'Shrub'
Final_predictions_RF$Land_Cover_majority[Final_predictions_RF$Land_Cover_majority == '7'] <- 'Bare ground'
Final_predictions_RF$Land_Cover_majority[Final_predictions_RF$Land_Cover_majority == '8'] <- 'Snow'

Final_predictions_RF$year <- format(as.Date(Final_predictions_RF$Year_M_D_H_M, format="%Y/%m/%D"),"%Y")

df_weekly_Est <- as.data.frame(matrix(NA, nrow=1,ncol=6))
colnames(df_weekly_Est) <- c("week_num", "mean_CH4","year",  "Data", "Type", "Land_cover_majority")
my_station <-  c('Sta',"Stn")
for(i in seq_along(my_station)){
  df_ <- Final_predictions_RF[Final_predictions_RF$Data==my_station[i],]
  num_years <- unique(df_$year)
  print(num_years)
  for(j in seq_along(num_years)){
    df <- df_[df_$year==num_years[j],]
    df$week_num <- strftime(df$Year_M_D_H_M, format = "%V")
    df2 <- df %>%
      group_by(week_num) %>%
      summarize(mean_CH4 = mean(Predicted_CH4))
    df2$year <- rep(num_years[j], nrow(df2))
    df2$Data <- rep(my_station[i], nrow(df2))
    df2$Type <- rep("Prediction", nrow(df2))
    df2$Land_cover_majority <- NA
    df2$week_num  <- as.numeric(df2$week_num)
    df_weekly_Est <- rbind(df_weekly_Est, df2)
  }
}
df_weekly_Est <- df_weekly_Est[-1,]
df_weekly_Obs <- as.data.frame(matrix(NA, nrow=1,ncol=6))
colnames(df_weekly_Obs) <- c("week_num", "mean_CH4","year",  "Data", "Type", "Land_cover_majority")
my_station <-  c('Sta',"Stn")
for(i in seq_along(my_station)){
  df_ <- Final_predictions_RF[Final_predictions_RF$Data==my_station[i],]
  num_years <- unique(df_$year)
  print(num_years)
  for(j in seq_along(num_years)){
    df <- df_[df_$year==num_years[j],]
    df$week_num <- strftime(df$Year_M_D_H_M, format = "%V")
    df2 <- df %>%
      group_by(week_num) %>%
      summarize(mean_CH4 = mean(Observed_CH4))
    df2$year <- rep(num_years[j], nrow(df2))
    df2$Data <- rep(my_station[i], nrow(df2))
    df2$Type <- rep("Observation", nrow(df2))
    df2$Land_cover_majority <- NA
    df2$week_num  <- as.numeric(df2$week_num)
    df_weekly_Obs <- rbind(df_weekly_Obs, df2)
  }
}
df_weekly_Obs <- df_weekly_Obs[-1,]
df_weekly <- rbind(df_weekly_Obs,df_weekly_Est)
Prediction_vs_Habitat_plot_TOTAL <- 
  ggplot(df_weekly,aes(x=week_num , y=mean_CH4, color=Data))+#facet_grid(cols = vars(Cluster))+
  geom_point(size = 2, alpha = 0.8) + # theme(legend.position="none")+
  labs(title = "Methane weekly Predictions" ,
       #  subtitle = "Density Plot",
       x = "Week", y = "CH4")+facet_grid(cols = vars(year), rows = vars(Type))+ theme_bw()+
  theme(axis.text.x = element_text(angle = 90, size = 18),  axis.title.x=element_text(size = 18), axis.text.y = element_text(size = 18),  axis.title.y = element_text(size = 18),
        legend.title = element_text( size = 18), legend.text = element_text( size = 18), 
        plot.title = element_text(size=22))
# scale_colour_manual(values = c("Water" = "#2166ac",
#                                 "Tree" =  "#858585",
#                                "Shrub" = "#B2182B",
#                               "Bare ground" = "#D1E5F0",
#                               "Snow"= "#D55E00"))

png(width = 1500, height = 700,paste0("/Users/ilhembouderbala/Downloads/Scaled_data-4/predictions_WEEKLY.png"),res=100)
print(Prediction_vs_Habitat_plot_TOTAL)
dev.off()



library(dplyr)
df <- Final_predictions_RF %>% 
  group_by(week = week(Year_M_D_H_M)) %>%
  mutate(Predicted_CH4_wk_average = mean(Predicted_CH4)) %>%
  ungroup() %>%
  group_by(month = month(date)) %>%
  mutate(Price.mo.average = mean(price))

df <-Final_predictions_RF %>% 
  group_by( Predicted_CH4_wk_average = paste( year(Year_M_D_H_M), week(Year_M_D_H_M)))





Residual_plot2 <- 
  ggplot(Final_predictions_RF,aes(x=Observed_CH4 , y=Residual, label = Data, color=Data))+#facet_grid(cols = vars(Cluster))+
  # geom_point(size = 2, alpha = 0.8) #+   facet_grid(Model ~ Data)+
  geom_smooth(method = "lm", formula = formula) +
  #stat_poly_eq(aes(label = after_stat(adj.rr.label)), formula = formula, 
  #             parse = TRUE,label.y = "bottom")+
  # stat_poly_eq(aes(label = after_stat(adj.rr.label)), formula = formula, 
  #              parse = TRUE,label.y = "bottom")+
  stat_smooth(aes(color=Data), method = "lm", formula = formula) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
    formula = formula, label.y = c(-2.5, -2.4, -2.3, -2.2, -2.1, -2, -1.9,-1.8)) + 
  #geom_text(data = Accuaary_final,
  #          mapping = aes(x = x, y = y, label = Label))+
  # geom_xsidedensity() +
  #geom_ysidedensity() +
  #scale_color_tq() +
  # scale_fill_tq() +
  #theme_tq() +
  labs(title = "Residuals vs Observed CH4 (predicted (RF)-observed)" ,
       #  subtitle = "Density Plot",
       x = "Observed CH4", y = "Residual")


png(width = 800, height = 600,paste0("/Users/ilhembouderbala/Dropbox/Mon disque/UAlberta/Air_pollution project/Data/Air_quality_data/Pure_normalized/results/Figures/Residual_plot2.png"),res=100)
print(Residual_plot2)
dev.off()


pred_A_B <- Final_predictions[Final_predictions$Model=="RF",] # rbind(Final_predictions[Final_predictions$Model=="RF_MODEL",], Final_predictions_Anzac[Final_predictions_Anzac$Model=="RF_MODEL",])
pred_A_B$Year_M_D_H_M <- as.Date( pred_A_B$Year_M_D_H_M)
pred_A_B$Land_Cover_majority[pred_A_B$Land_Cover_majority == '0'] <- 'Water'
pred_A_B$Land_Cover_majority[pred_A_B$Land_Cover_majority == '1'] <- 'Tree'
pred_A_B$Land_Cover_majority[pred_A_B$Land_Cover_majority == '5'] <- 'Shrub'
pred_A_B$Land_Cover_majority[pred_A_B$Land_Cover_majority == '7'] <- 'Bare ground'
pred_A_B$Land_Cover_majority[pred_A_B$Land_Cover_majority == '8'] <- 'Snow'

library(dplyr)
df <- pred_A_B %>% 
  group_by(week = week(Year_M_D_H_M)) %>%
  mutate(Price.wk.average = mean(price)) %>%
  ungroup() %>%
  group_by(month = month(date)) %>%
  mutate(Price.mo.average = mean(price))




Prediction_vs_Habitat_plot_Mannxi_Anzac <- 
  ggplot(pred_A_B,aes(x= Index , y= Predicted_CH4 , label = Land_Cover_majority, color = Land_Cover_majority))+#facet_grid(cols = vars(Cluster))+
  geom_point(size = 2, alpha = 0.8) +facet_grid(cols = vars(Data),scales= "free" )+# xlim(1.85,4) #theme(legend.position="none")+
  
  #  geom_line(size = 2, alpha = 0.8, aes(x= Observed_CH4 , y=Year_M_D_H_M)) + #theme(legend.position="none")+
  
  labs(title = "Predictions based on habitat (Buffalo vs Anzac)" ,
       #  subtitle = "Density Plot",
       x = "Time", y = "Predicted CH4")



#Prediction_plots_ <- plot_grid(Prediction_vs_Habitat_plot_Mildred_lake, 
#                              Prediction_vs_Habitat_plot_Anzac, 
#          nrow = 1, ncol=2, rel_widths = c(0.5, 0.5), label_size = 20)
#Prediction_plots <- plot_grid(Prediction_plots_, 
#                              Prediction_vs_Habitat_plot_Stacked, 
#                              nrow = 2, ncol=1, rel_heights = c(0.5, 0.5), label_size = 20)

png(width = 1000, height = 600,paste0("/Users/ilhembouderbala/Dropbox/Mon disque/UAlberta/Air_pollution project/Data/Air_quality_data/Pure_normalized/results/Figures/predictions_based_on_land_cover.png"),res=100)
print(Prediction_vs_Habitat_plot_Mannxi_Anzac)
dev.off()



Land_cover_by_station <- as.data.frame(matrix(NA, nrow=7*4, ncol=3))
colnames(Land_cover_by_station) <- c("Land_cover", "Station", "Count")
Land_cover_by_station$Land_cover <- rep(c("0", "1", "5", "8"), 7)
Unique_station <- unique(Final_predictions$Station)
Land_cover <- c("0", "1", "5", "8")
Land_cover_by_station$Station <- c(rep(Unique_station[1], 4),rep(Unique_station[2], 4),rep(Unique_station[3], 4),rep(Unique_station[4], 4),
                                   rep(Unique_station[5], 4),rep(Unique_station[6], 4),rep(Unique_station[7], 4))

for(i in seq_along(Unique_station)){
  for(j in seq_along(Land_cover)){
    Land_cover_by_station[c(Land_cover_by_station$Station==Unique_station[i] & Land_cover_by_station$Land_cover==Land_cover[j]), "Count"] <- nrow(Data_stack[c(Data_stack$Land_Cover==Land_cover[j] & Data_stack$name==Unique_station[i]),])
  }
}
bar_chart_land_cover <- ggplot(Land_cover_by_station, aes(x =Land_cover, y=Count, colour= Land_cover, fill =Land_cover))+
  geom_bar(stat="identity", alpha=0.4,  position=position_dodge())+
  labs(title = "Land cover count per station" ,
       #  subtitle = "Density Plot",
       x = "Land cover", y = "Count")+ facet_grid(cols = vars(Station),scales= "free" )

png(width = 1500, height = 600,paste0("/Users/ilhembouderbala/Dropbox/Mon disque/UAlberta/Air_pollution project/Data/Air_quality_data/Pure_normalized/results/Figures/bar_chart_land_cover.png"),res=100)
print(bar_chart_land_cover)
dev.off()

