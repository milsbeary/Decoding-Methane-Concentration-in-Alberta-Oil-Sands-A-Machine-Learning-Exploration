#TABLE COMPARE FOR SPEARMAN AND PEARSON COEFICENT IN THE MODELS FOR VARIABLE IMPORTANCE SELECTION 
#"DECODING METHANE CONCENTRATIONS IN ALBERTA OIL SANDS: A MACHINE LEARNING EXPLORATION"

Run 1._start_up_code.R 
Run 2._data_upload_and_cleaning.R

#Variable importance function with pearson corrilation coefficent
select07_Logit_p <- function(pred_names, response_name, data, threshold=thre){
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
  cor_mat <- cor(data[pred_names], method='pearson')
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

#Variable importance function with spearman correlation coefficent
select07_Logit_s <- function(pred_names, response_name, data, threshold=thre){
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

#Compare dataframes in R by enteries. creates a dataframe of enteries where they are different 
##Assumes both dataframes are the same size
#data_frame_comparer <- function(df1, df2) { 
#	df_rows = nrow(df1)
#	df_cols = ncol(df1)
#	matching_df <- matrix(ncol = length(df_cols), nrow = length(df_rows))
#	for (i in 1:df_rows) { 
#		for (j in 1:df_cols) { 
#			df_1_entry <- df1[i, j]
#			df_2_entry <- df2[i, j]
#			if (df_1_entry == df_2_entry) { 
#				matching_df[i, j] = "same" 
#			} else { 
#				matching_df[i, j] = "different" 
#			}	
#		}	
#	}
#	return(matching_df)	
#}	

#Dataframe names

df_nms <- c("Anz_nlc", "Ath_nlc", "Ber_nlc", "Buf_nlc", "Con_nlc", "Jan_nlc", "Low_nlc", "Man_nlc", "Mil_nlc", "Pat_nlc", "Sta_nlc", "Stn_nlc", "Sto_nlc")
#df_nms <- c("Anz_slc", "Ath_slc", "Ber_slc", "Buf_slc", "Con_slc", "Jan_slc", "Low_slc", "Man_slc", "Mil_slc", "Pat_slc", "Sta_slc", "Stn_slc", "Sto_slc","Anz_nlc", "Ath_nlc", "Ber_nlc", "Buf_nlc", "Con_nlc", "Jan_nlc", "Low_nlc", "Man_nlc", "Mil_nlc", "Pat_nlc", "Sta_nlc", "Stn_nlc", "Sto_nlc", "Anz_lcm", "Ath_lcm", "Ber_lcm", "Buf_lcm", "Con_lcm", "Jan_lcm", "Low_lcm", "Man_lcm", "Mil_lcm", "Pat_lcm", "Sta_lcm", "Stn_lcm", "Sto_lcm")


#Squares and transformations

squares_in <- c(1)
squares_in_names <- c("exp_one")
trans_in <- c(-2, -1, 2)
trans_in_names <- c("exp_neg_two", "exp_neg_one", "exp_two")


#Names of groups

group_nms <- c("scl", "nlc", "lcm")


#Vector of normed or scaled

filt_nms <- c("scl", "nrm")	

#Plain
for (i in 1:length(df_list)) {
	
	#i=1
	print(i)
	print(df_nms[i])
	#head(df_list[[i]])
	
	var_imp_frame[i, "ScalOrNorm"] <- filt_nms[2] #Telling us if scaled or normed 
	
	df_full <- data.frame(df_list[[i]]) #Extracting dataframe and defining as a variable
		
	#Getting data ready for top ten selection
	char_frame <- df_full%>%dplyr::select(where(is.character)) #Getting a dataframe of character variables
	num_frame <- df_full%>%dplyr::select(where(is.numeric))%>%dplyr::select(-c("CH4..ppm."))	 #Getting a dataframe for numerical predictors
	num_fcnames <- colnames(num_frame) #Getting list of dataframe columns for later in the loop
	meth_frame <- df_full%>%dplyr::select(c("CH4..ppm.")) #Getting a dataframe for the numerical dependent

	#Removing variables that will not normalize (to be added back later)
	counter <- c(linspace(1, nrow(num_frame), nrow(num_frame)))
	zero_mean_df <- data.frame(counter) #Create a blank dataframe to append to
	for (j in 1:length(num_fcnames)) { 
		df_colname <- as.character(num_fcnames[j])
		df_colmean <- mean(num_frame[,df_colname])
		if (df_colmean == 0) { 
			zero_mean_df[[df_colname]] <- num_frame[[df_colname]] #Add it to a seperate dataframe so it 
			num_frame[[df_colname]] <- NULL #Remove dataframe column so it can be added back
		} else if (is.na(df_colmean)) { 
			num_frame[[df_colname]] <- NULL			
		} else { 
			#Do nothing
		}
	}
	#summary(zero_mean_df)
	#summary(num_frame)			
	
	#Normalizing the data
	num_frame <- num_frame%>%mutate((across(everything(), num_normalizer)))
	#summary(num_frame)
	meth_frame <- meth_frame%>%mutate((across(everything(), num_normalizer)))
	#summary(meth_frame)
	def_frame_full <- data.frame(cbind(num_frame, meth_frame))
	#summary(def_frame_full)
	
	#Adding back numerical data that could not be normalized
	#if (ncol(zero_mean_df) != 1) { 
	#	def_frame_full <- data.frame(cbind(def_frame_full, zero_mean_df%>%dplyr::select(-c("counter"))))
	#	}
	#summary(def_frame_full)
	
	#Generating variable importance for spearman
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sel <- c() #Empty vector to append to
	var_selp <- select07_Logit_p(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logitp <- c(var_selp[["pred_sel"]]) #Getting the list of predictors
	#print(length(var_sel[["pred_sel"]])) 
	my_preds_Logitp <- my_preds_Logitp[1:15] #Selecting top 15
		
	#Generating variable importance for spearman 
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sels <- c() #Empty vector to append to
	var_sels <- select07_Logit_s(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logits <- c(var_sels[["pred_sel"]]) #Getting the list of predictors
	#print(length(var_sel[["pred_sel"]])) 
	my_preds_Logits <- my_preds_Logits[1:15] #Selecting top 15
			  		 						  		 
	#summary(df_to_out)
	print(identical(my_preds_Logits, my_preds_Logitp))
	
	if (identical(my_preds_Logits, my_preds_Logitp) == FALSE) { 
		
		print(my_preds_Logitp)
		print(my_preds_Logits)
		
		}
		
} #End of loop


#Trans 
for (i in 1:length(df_list)) {
	
	#i=1
	print(i)
	print(df_nms[i])
	#head(df_list[[i]])
	
	var_imp_frame[i, "ScalOrNorm"] <- filt_nms[2] #Telling us if scaled or normed 
	
	df_full <- data.frame(df_list[[i]]) #Extracting dataframe and defining as a variable
		
	#Getting data ready for top ten selection
	char_frame <- df_full%>%dplyr::select(where(is.character)) #Getting a dataframe of character variables
	num_frame <- df_full%>%dplyr::select(where(is.numeric))%>%dplyr::select(-c("CH4..ppm."))	 #Getting a dataframe for numerical predictors
	num_fcnames <- colnames(num_frame) #Getting list of dataframe columns for later in the loop
	meth_frame <- df_full%>%dplyr::select(c("CH4..ppm.")) #Getting a dataframe for the numerical dependent

	#Removing variables that will not normalize (to be added back later)
	counter <- c(linspace(1, nrow(num_frame), nrow(num_frame)))
	zero_mean_df <- data.frame(counter) #Create a blank dataframe to append to
	for (j in 1:length(num_fcnames)) { 
		df_colname <- as.character(num_fcnames[j])
		df_colmean <- mean(num_frame[,df_colname])
		if (df_colmean == 0) { 
			zero_mean_df[[df_colname]] <- num_frame[[df_colname]] #Add it to a seperate dataframe so it 
			num_frame[[df_colname]] <- NULL #Remove dataframe column so it can be added back
		} else if (is.na(df_colmean)) { 
			num_frame[[df_colname]] <- NULL			
		} else { 
			#Do nothing
		}
	}
	#summary(zero_mean_df)
	#summary(num_frame)
	
	num_frame <- data.frame(column_transformer(num_frame, trans_in,  trans_in_names)) #Tranforming everything

	#Normalizing the data
	num_frame <- num_frame%>%mutate((across(everything(), num_normalizer)))%>%
			 				select(where(~ all(!is.na(.)))) #Remove columns with NA values if they exist
	#summary(num_frame)
	meth_frame <- meth_frame%>%mutate((across(everything(), num_normalizer)))
	#summary(meth_frame)
	def_frame_full <- data.frame(cbind(num_frame, meth_frame))
	#summary(def_frame_full)
	
	#Adding back numerical data that could not be normalized
	#if (ncol(zero_mean_df) != 1) { 
	#	def_frame_full <- data.frame(cbind(def_frame_full, zero_mean_df%>%dplyr::select(-c("counter"))))
	#	}
	#summary(def_frame_full)
	
	#Generating variable importance for spearman
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sel <- c() #Empty vector to append to
	var_selp <- select07_Logit_p(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logitp <- c(var_selp[["pred_sel"]]) #Getting the list of predictors
	#print(length(var_sel[["pred_sel"]])) 
	my_preds_Logitp <- my_preds_Logitp[1:15] #Selecting top 15
		
	#Generating variable importance for spearman 
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sels <- c() #Empty vector to append to
	var_sels <- select07_Logit_s(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logits <- c(var_sels[["pred_sel"]]) #Getting the list of predictors
	#print(length(var_sel[["pred_sel"]])) 
	my_preds_Logits <- my_preds_Logits[1:15] #Selecting top 15
			  		 						  		 
	#summary(df_to_out)
	print(identical(my_preds_Logits, my_preds_Logitp))
	
	if (identical(my_preds_Logits, my_preds_Logitp) == FALSE) { 
		
		print(my_preds_Logitp)
		print(my_preds_Logits)
		
		}

} #End of loop

#Interact
for (i in 1:length(df_list)) {
	
	#i=1
	print(i)
	print(df_nms[i])
	#head(df_list[[i]])
	
	var_imp_frame[i, "ScalOrNorm"] <- filt_nms[2] #Telling us if scaled or normed 
	
	df_full <- data.frame(df_list[[i]]) #Extracting dataframe and defining as a variable
		
	#Getting data ready for top ten selection
	char_frame <- df_full%>%dplyr::select(where(is.character)) #Getting a dataframe of character variables
	num_frame <- df_full%>%dplyr::select(where(is.numeric))%>%dplyr::select(-c("CH4..ppm."))	 #Getting a dataframe for numerical predictors
	num_fcnames <- colnames(num_frame) #Getting list of dataframe columns for later in the loop
	meth_frame <- df_full%>%dplyr::select(c("CH4..ppm.")) #Getting a dataframe for the numerical dependent

	#Removing variables that will not normalize (to be added back later)
	counter <- c(linspace(1, nrow(num_frame), nrow(num_frame)))
	zero_mean_df <- data.frame(counter) #Create a blank dataframe to append to
	for (j in 1:length(num_fcnames)) { 
		df_colname <- as.character(num_fcnames[j])
		df_colmean <- mean(num_frame[,df_colname])
		if (df_colmean == 0) { 
			zero_mean_df[[df_colname]] <- num_frame[[df_colname]] #Add it to a seperate dataframe so it 
			num_frame[[df_colname]] <- NULL #Remove dataframe column so it can be added back
		} else if (is.na(df_colmean)) { 
			num_frame[[df_colname]] <- NULL			
		} else { 
			#Do nothing
		}
	}
	#summary(zero_mean_df)
	#summary(num_frame)
	
	num_frame <- data.frame(column_interacter(num_frame, squares_in,  squares_in_names)) #Interacting everything 
	
	#Normalizing the data
	num_frame <- num_frame%>%mutate((across(everything(), num_normalizer)))%>%
			 				select(where(~ all(!is.na(.)))) #Remove columns with NA values if they exist
	#summary(num_frame)
	meth_frame <- meth_frame%>%mutate((across(everything(), num_normalizer)))
	#summary(meth_frame)
	def_frame_full <- data.frame(cbind(num_frame, meth_frame))
	#summary(def_frame_full)
	
	#Adding back numerical data that could not be normalized
	#if (ncol(zero_mean_df) != 1) { 
	#	def_frame_full <- data.frame(cbind(def_frame_full, zero_mean_df%>%dplyr::select(-c("counter"))))
	#	}
	#summary(def_frame_full)
	
	#Generating variable importance for spearman
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sel <- c() #Empty vector to append to
	var_selp <- select07_Logit_p(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logitp <- c(var_selp[["pred_sel"]]) #Getting the list of predictors
	#print(length(var_sel[["pred_sel"]])) 
	my_preds_Logitp <- my_preds_Logitp[1:15] #Selecting top 15
		
	#Generating variable importance for spearman 
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sels <- c() #Empty vector to append to
	var_sels <- select07_Logit_s(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logits <- c(var_sels[["pred_sel"]]) #Getting the list of predictors
	#print(length(var_sel[["pred_sel"]])) 
	my_preds_Logits <- my_preds_Logits[1:15] #Selecting top 15
			  		 						  		 
	#summary(df_to_out)
	print(identical(my_preds_Logits, my_preds_Logitp))
	
	if (identical(my_preds_Logits, my_preds_Logitp) == FALSE) { 
		
		print(my_preds_Logitp)
		print(my_preds_Logits)
		
		}
	
} #End of loop


#Everything

#Blank dataframe for variable importance
imp_cnames <- c("df", "Var_1", "Var_2", "Var_3", "Var_4", "Var_5", "Var_6", "Var_7", "Var_8", "Var_9", "Var_10", "Var_11", "Var_12", "Var_13", "Var_14", "Var_15", "ScalOrNorm", "Group_type")
var_imp_frame <- data.frame(matrix(ncol = length(imp_cnames), nrow = length(df_nms)))
colnames(var_imp_frame) <- imp_cnames

for (i in 1:length(df_list)) {
	
	print(i)
	print(df_nms[i])
	
	var_imp_frame[i, "ScalOrNorm"] <- filt_nms[2] #Telling us if scaled or normed 
	
	df_full <- data.frame(df_list[[i]]) #Extracting dataframe and defining as a variable
		
	#Getting data ready for top ten selection
	char_frame <- df_full%>%dplyr::select(where(is.character)) #Getting a dataframe of character variables
	num_frame <- df_full%>%dplyr::select(where(is.numeric))%>%dplyr::select(-c("CH4..ppm."))	 #Getting a dataframe for numerical predictors
	num_fcnames <- colnames(num_frame) #Getting list of dataframe columns for later in the loop
	meth_frame <- df_full%>%dplyr::select(c("CH4..ppm.")) #Getting a dataframe for the numerical dependent

	#Removing variables that will not normalize (to be added back later)
	counter <- c(linspace(1, nrow(num_frame), nrow(num_frame)))
	zero_mean_df <- data.frame(counter) #Create a blank dataframe to append to
	for (j in 1:length(num_fcnames)) { 
		df_colname <- as.character(num_fcnames[j])
		df_colmean <- mean(num_frame[,df_colname])
		if (df_colmean == 0) { 
			zero_mean_df[[df_colname]] <- num_frame[[df_colname]] #Add it to a seperate dataframe so it 
			num_frame[[df_colname]] <- NULL #Remove dataframe column so it can be added back
		} else if (is.na(df_colmean)) { 
			num_frame[[df_colname]] <- NULL			
		} else { 
			#Do nothing
		}
	}
	#summary(zero_mean_df)
	#summary(num_frame)
	
	num_frame_int <- data.frame(column_interacter(num_frame, squares_in,  squares_in_names))#Interacting everything
	num_frame_trans <- data.frame(column_transformer(num_frame, trans_in,  trans_in_names)) #Tranforming everything
	same_names <- intersect(names(num_frame_int), names(num_frame_trans)) #See which variable names are the same
	num_frame <- data.frame(cbind(num_frame_int, num_frame_trans%>%dplyr::select(-all_of(same_names)))) #Combining together without duplicate columns 
	
	#Normalizing the data
	num_frame <- num_frame%>%mutate((across(everything(), num_normalizer)))%>%
			 				select(where(~ all(!is.na(.)))) #Remove columns with NA values if they exist
	#summary(num_frame)
	meth_frame <- meth_frame%>%mutate((across(everything(), num_normalizer)))
	#summary(meth_frame)
	def_frame_full <- data.frame(cbind(num_frame, meth_frame))
	#summary(def_frame_full)
	
	#Adding back numerical data that could not be normalized
	#if (ncol(zero_mean_df) != 1) { 
	#	def_frame_full <- data.frame(cbind(def_frame_full, zero_mean_df%>%dplyr::select(-c("counter"))))
	#	}
	#summary(def_frame_full)

	#Generating variable importance for spearman
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sel <- c() #Empty vector to append to
	var_selp <- select07_Logit_p(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logitp <- c(var_selp[["pred_sel"]]) #Getting the list of predictors
	#print(length(var_sel[["pred_sel"]])) 
	my_preds_Logitp <- my_preds_Logitp[1:15] #Selecting top 15
		
	#Generating variable importance for spearman 
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sels <- c() #Empty vector to append to
	var_sels <- select07_Logit_s(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logits <- c(var_sels[["pred_sel"]]) #Getting the list of predictors
	#print(length(var_sel[["pred_sel"]])) 
	my_preds_Logits <- my_preds_Logits[1:15] #Selecting top 15
			  		 						  		 
	#summary(df_to_out)
	print(identical(my_preds_Logits, my_preds_Logitp))
	
	if (identical(my_preds_Logits, my_preds_Logitp) == FALSE) { 
		
		print(my_preds_Logitp)
		print(my_preds_Logits)
		
		}

} #End of loop












