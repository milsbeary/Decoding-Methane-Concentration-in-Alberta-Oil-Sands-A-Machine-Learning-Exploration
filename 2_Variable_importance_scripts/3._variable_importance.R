#DETERMINES TOP 15 MOST IMPORTANT VARIABLES FOR ALL GROUPS AND EXPORTS TABLES AS CSV
#"DECODING METHANE CONCENTRATIONS IN ALBERTA OIL SANDS: A MACHINE LEARNING EXPLORATION" 

Run 1._start_up_code.R 
Run 2._data_upload_and_cleaning.R



####ORGANIZING DATA AND DEFINING NEEDED VECTORS####


#Dataframes

df_list <- list(Anz_nlc, Ath_nlc, Ber_nlc, Buf_nlc, Con_nlc, Jan_nlc, Low_nlc, Man_nlc, Mil_nlc, Pat_nlc, Sta_nlc, Stn_nlc, Sto_nlc)
#df_list <- list(Anz_slc, Ath_slc, Ber_slc, Buf_slc, Con_slc, Jan_slc, Low_slc, Man_slc, Mil_slc, Pat_slc,  Sta_slc, Stn_slc, Sto_slc, Anz_nlc, Ath_nlc, Ber_nlc, Buf_nlc, Con_nlc, Jan_nlc, Low_nlc, Man_nlc, Mil_nlc, Pat_nlc, Sta_nlc, Stn_nlc, Sto_nlc, Anz_lcm, Ath_lcm, Ber_nlc, Buf_lcm, Con_lcm, Jan_lcm, Low_lcm, Man_lcm, Mil_lcm, Pat_lcm, Sta_lcm, Stn_lcm, Sto_lcm)


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



#####COMPUTING VARIABLE IMPORTANCE####


#PURE NORMED#

#Building blank dataframe for variable importance
imp_cnames <- c("df", "Var_1", "Var_2", "Var_3", "Var_4", "Var_5", "Var_6", "Var_7", "Var_8", "Var_9", "Var_10", "Var_11", "Var_12", "Var_13", "Var_14", "Var_15", "ScalOrNorm", "Group_type")
var_imp_frame <- data.frame(matrix(ncol = length(imp_cnames), nrow = length(df_nms)))
colnames(var_imp_frame) <- imp_cnames

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
	
	#Generating variable importance
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sel <- c() #Empty vector to append to
	var_sel <- select07_Logit(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logit<- c(var_sel[["pred_sel"]]) #Getting the list of predictors
	#print(length(var_sel[["pred_sel"]])) 
	my_preds_Logit <- my_preds_Logit[1:15] #Selecting top 15
	print(my_preds_Logit) #See the top 15 most important variables	
	#print(ncol(num_frame))
	#Appending results to variable importance dataframes
	inp_vec <- append(df_nms[i], my_preds_Logit) #Adding the predictors to the dataframe vector
	var_imp_frame[i, linspace(1,16,16)] <- inp_vec #Adding to the variable importance dataframe
	var_imp_frame[i, "Group_type"] <-  str_split(df_nms[i], "_")[[1]][2] #Add data type to variable importace tataframe

	#Building dataframes from the results
	cols_keep_f <- append(na.omit(my_preds_Logit), "CH4..ppm.") #Getting list of collumns to keep
	df_to_out <- def_frame_full%>%dplyr::select(all_of(cols_keep_f))%>%
								  mutate(yyyy.MM.dd.HH.mm = char_frame$yyyy.MM.dd.HH.mm, 
								  		 name = char_frame$name)
	#summary(df_to_out)

	#write.csv(df_to_out, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/Pure/top_15/", df_nms[i], "_top_15.csv", sep = "")) #Exporting smaller csv
	
} #End of loop

#write.csv(var_imp_frame, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/Pure/pure_variable_importance.csv", sep = "")) #Exporting variable importance csv

#write.csv(var_imp_frame, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/var_import/Dirty/pure_variable_importance.csv", sep = "")) #Exporting variable importance to another folder for name cleaning



#TRANS#

#Blank dataframe for variable importance
imp_cnames <- c("df", "Var_1", "Var_2", "Var_3", "Var_4", "Var_5", "Var_6", "Var_7", "Var_8", "Var_9", "Var_10", "Var_11", "Var_12", "Var_13", "Var_14", "Var_15", "ScalOrNorm", "Group_type")
var_imp_frame <- data.frame(matrix(ncol = length(imp_cnames), nrow = length(df_nms)))
colnames(var_imp_frame) <- imp_cnames

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
	
	#Generating variable importance
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sel <- c() #Empty vector to append to
	var_sel <- select07_Logit(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logit<- c(var_sel[["pred_sel"]]) #Getting the list of predictors
	my_preds_Logit <- my_preds_Logit[1:15] #Selecting top 15
	print(my_preds_Logit) #See the top 15 most important variables
	
	#Appending results to variable importance dataframes
	inp_vec <- append(df_nms[i], my_preds_Logit) #Adding the predictors to the dataframe vector
	var_imp_frame[i, linspace(1,16,16)] <- inp_vec #Adding to the variable importance dataframe
	var_imp_frame[i, "Group_type"] <-  str_split(df_nms[i], "_")[[1]][2] #Add data type to variable importace tataframe

	#Building dataframes from the results
	cols_keep_f <- append(na.omit(my_preds_Logit), "CH4..ppm.") #Getting list of collumns to keep
	df_to_out <- def_frame_full%>%dplyr::select(all_of(cols_keep_f))%>%
								  mutate(yyyy.MM.dd.HH.mm = char_frame$yyyy.MM.dd.HH.mm, 
								  		 name = char_frame$name)
	#summary(df_to_out)

	#write.csv(df_to_out, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/Trans/top_15/", df_nms[i], "_top_15.csv", sep = "")) #Exporting smaller csv

} #End of loop

#write.csv(var_imp_frame, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/Trans/trans_variable_importance.csv", sep = "")) #Exporting variable importance csv

#write.csv(var_imp_frame, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/var_import/Dirty/trans_variable_importance.csv", sep = "")) #Exporting variable importance to another folder for name cleaning


#INTERACT#

#Blank dataframe for variable importance
imp_cnames <- c("df", "Var_1", "Var_2", "Var_3", "Var_4", "Var_5", "Var_6", "Var_7", "Var_8", "Var_9", "Var_10", "Var_11", "Var_12", "Var_13", "Var_14", "Var_15", "ScalOrNorm", "Group_type")
var_imp_frame <- data.frame(matrix(ncol = length(imp_cnames), nrow = length(df_nms)))
colnames(var_imp_frame) <- imp_cnames

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
	
	#Generating variable importance
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sel <- c() #Empty vector to append to
	var_sel <- select07_Logit(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logit<- c(var_sel[["pred_sel"]]) #Getting the list of predictors
	my_preds_Logit <- my_preds_Logit[1:15] #Selecting top 15
	print(my_preds_Logit) #See the top 15 most important variables
	
	#Appending results to variable importance dataframes
	inp_vec <- append(df_nms[i], my_preds_Logit) #Adding the predictors to the dataframe vector
	var_imp_frame[i, linspace(1,16,16)] <- inp_vec #Adding to the variable importance dataframe
	var_imp_frame[i, "Group_type"] <-  str_split(df_nms[i], "_")[[1]][2] #Add data type to variable importace tataframe

	#Building dataframes from the results
	cols_keep_f <- append(na.omit(my_preds_Logit), "CH4..ppm.") #Getting list of collumns to keep
	df_to_out <- def_frame_full%>%dplyr::select(all_of(cols_keep_f))%>%
								  mutate(yyyy.MM.dd.HH.mm = char_frame$yyyy.MM.dd.HH.mm, 
								  		 name = char_frame$name)
	#summary(df_to_out)

	#write.csv(df_to_out, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/Interact/top_15/", df_nms[i], "_top_15.csv", sep = "")) #Exporting smaller csv
	
} #End of loop

#write.csv(var_imp_frame, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/Interact/interact_variable_importance.csv", sep = "")) #Exporting variable importance csv

#write.csv(var_imp_frame, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/var_import/Dirty/interact_variable_importance.csv", sep = "")) #Exporting variable importance to another file for name cleaing


####EVERYTHING####

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

	#Generating variable importance
	my_preds <- colnames(def_frame_full%>%dplyr::select(-c("CH4..ppm."))) #Get a list of predictors for variable importance
	var_sel <- c() #Empty vector to append to
	var_sel <- select07_Logit(pred_names=my_preds, response_name="CH4..ppm.", data=def_frame_full, threshold=0.6) #Running variable importance function
	my_preds_Logit<- c(var_sel[["pred_sel"]]) #Getting the list of predictors
	my_preds_Logit <- my_preds_Logit[1:15] #Selecting top 15
	print(my_preds_Logit) #See the top 15 most important variables
	
	#Appending results to variable importance dataframes
	inp_vec <- append(df_nms[i], my_preds_Logit) #Adding the predictors to the dataframe vector
	var_imp_frame[i, linspace(1,16,16)] <- inp_vec #Adding to the variable importance dataframe
	var_imp_frame[i, "Group_type"] <-  str_split(df_nms[i], "_")[[1]][2] #Add data type to variable importace tataframe

	#Building dataframes from the results
	cols_keep_f <- append(na.omit(my_preds_Logit), "CH4..ppm.") #Getting list of collumns to keep
	df_to_out <- def_frame_full%>%dplyr::select(all_of(cols_keep_f))%>%
								  mutate(yyyy.MM.dd.HH.mm = char_frame$yyyy.MM.dd.HH.mm, 
								  		 name = char_frame$name)
	#summary(df_to_out)

	#write.csv(df_to_out, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/Everything/top_15/", df_nms[i], "_top_15.csv", sep = "")) #Exporting smaller csv

} #End of loop

#write.csv(var_imp_frame, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/Everything/everything_variable_importance.csv", sep = "")) #Exporting variable importance csv


#write.csv(var_imp_frame, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/var_import/Dirty/everything_variable_importance.csv", sep = "")) #Exporting variable importance to another file for name cleaning 



