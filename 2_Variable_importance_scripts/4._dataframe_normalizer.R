#GENERATES NEEDED NORMALIZED DATAFRAMES FOR FURTHER RESULTS 
#"DECODING METHANE CONCENTRATIONS IN ALBERTA OIL SANDS: A MACHINE LEARNING EXPLORATION"

Run 1._start_up_code.R 
Run 2._data_upload_and_cleaning.R


####ORGANIZING DATA AND DEFINING NEEDED VECTORS####


#Dataframes

df_list <- list(Anz_nlcA, Ath_nlcA, Ber_nlcA, Buf_nlcA, Con_nlcA, Jan_nlcA, Low_nlcA, Man_nlcA, Mil_nlcA, Pat_nlcA, Sta_nlcA, Stn_nlcA, Sto_nlcA)
#df_list <- list(Anz_slcA, Ath_slcA, Ber_slcA, Buf_slcA, Con_slcA, Jan_slcA, Low_slcA, Man_slcA, Mil_slcA, Pat_slcA, Sta_slcA, Stn_slcA,  Sto_slcA, Anz_nlcA, Ath_nlcA, Ber_nlcA, Buf_nlcA, Con_nlcA, Jan_nlcA, Low_nlcA, Man_nlcA, Mil_nlcA, Pat_nlcA, Sta_nlcA, Stn_nlcA, Sto_nlcA, Anz_nlcA, Ath_lcmA, Ber_nlcA, Buf_lcmA, Con_lcmA, Jan_lcmA, Low_lcmA, Man_lcmA, Mil_lcmA, Pat_lcmA, Sta_lcmA, Stn_lcmA, Sto_lcmA)

#Dataframe names

df_nms <- c("Anz_nlc", "Ath_nlc", "Ber_nlc", "Buf_nlc", "Con_nlc", "Jan_nlc", "Low_nlc", "Man_nlc", "Mil_nlc", "Pat_nlc", "Sta_nlc", "Stn_nlc", "Sto_nlc")
#df_nms <- c("Anz_slc", "Ath_slc", "Ber_slc", "Buf_slc", "Con_slc", "Jan_slc", "Low_slc", "Man_slc", "Mil_slc", "Pat_slc", "Sta_slc", "Stn_slc", "Sto_slc", "Anz_nlc", "Ath_nlc", "Ber_nlc", "Buf_nlc", "Con_nlc", "Jan_nlc", "Low_nlc", "Man_nlc", "Mil_nlc", "Pat_nlc", "Sta_nlc", "Stn_nlc", "Sto_nlc", "Anz_lcm", "Ath_lcm", "Ber_lcm", "Buf_lcm", "Con_lcm", "Jan_lcm", "Low_lcm", "Man_lcm", "Mil_lcm", "Pat_lcm", "Sta_lcm", "Stn_lcm", "Sto_lcm")


####NORMALIZING AND EXPORTING####

for (i in 1:length(df_list)) {
	
	print(i)
	print(df_nms[i])
	
	#Creating needed dataframes	and seperating into what we need 
	
	#Full
	df_full <- data.frame(df_list[[i]]) #Extracting dataframe and defining as a variable
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
	
	#Normalizing the data
	num_frame <- num_frame%>%mutate((across(everything(), num_normalizer)))
	#summary(num_frame)
	meth_frame <- meth_frame%>%mutate((across(everything(), num_normalizer)))
	#summary(meth_frame)
	def_frame_full <- data.frame(cbind(num_frame, meth_frame))%>%select_if(~ !any(is.na(.)))
	#summary(def_frame_full)
	
	#Adding back numerical data that could not be normalized
	if (ncol(zero_mean_df) != 1) { 
		def_frame_full <- data.frame(cbind(def_frame_full, zero_mean_df%>%dplyr::select(-c("counter"))))
		}
	#def_frame_full <- data.frame(cbind(def_frame_full, char_frame)) #Adding character frames back
	
	#Exporting the data
	#write.csv(def_frame_full, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_dataframes/Full/", df_nms[i], "_full_norm.csv", sep = ""))

	#2019_2022
	df_2019_2022 <- data.frame(df_list[[i]])%>%filter(yyyy.MM.dd.HH.mm < "2023-01-01 00:00")
	char_frame_2019_2022 <- df_2019_2022%>%dplyr::select(where(is.character)) #Getting a dataframe of character variables
	num_frame_2019_2022 <- df_2019_2022%>%dplyr::select(where(is.numeric))%>%dplyr::select(-c("CH4..ppm."))	 #Getting a dataframe for numerical predictors
	num_fcnames_2019_2022 <- colnames(num_frame_2019_2022) #Getting list of dataframe columns for later in the loop
	meth_frame_2019_2022 <- df_2019_2022%>%dplyr::select(c("CH4..ppm.")) #Getting a dataframe for the numerical dependent

	#Removing variables that will not normalize (to be added back later)
	counter_2019_2022 <- c(linspace(1, nrow(num_frame_2019_2022), nrow(num_frame_2019_2022)))
	zero_mean_df_2019_2022 <- data.frame(counter_2019_2022) #Create a blank dataframe to append to
	for (j in 1:length(num_fcnames_2019_2022)) { 
		df_colname_2019_2022 <- as.character(num_fcnames_2019_2022[j])
		df_colmean_2019_2022 <- mean(num_frame_2019_2022[,df_colname_2019_2022])
		if (df_colmean_2019_2022 == 0) { 
			zero_mean_df_2019_2022[[df_colname_2019_2022]] <- num_frame_2019_2022[[df_colname_2019_2022]] #Add it to a seperate dataframe so it 
			num_frame_2019_2022[[df_colname_2019_2022]] <- NULL #Remove dataframe column so it can be added back
		} else if (is.na(df_colmean_2019_2022)) { 
			num_frame[[df_colname_2019_2022]] <- NULL			
		} else { 
			#Do nothing
		}
	}

	#Normalizing the data
	num_frame_2019_2022 <- num_frame_2019_2022%>%mutate((across(everything(), num_normalizer)))
	#summary(num_frame_2019_2022)
	meth_frame_2019_2022 <- meth_frame_2019_2022%>%mutate((across(everything(), num_normalizer)))
	#summary(meth_frame_2019_2022)
	def_frame_full_2019_2022 <- data.frame(cbind(num_frame_2019_2022, meth_frame_2019_2022))%>%select_if(~ !any(is.na(.)))
	#summary(def_frame_full_2019_2022)
	
	#Adding back numerical data that could not be normalized
	if (ncol(zero_mean_df_2019_2022) != 1) { 
		def_frame_full_2019_2022 <- data.frame(cbind(def_frame_full_2019_2022, zero_mean_df_2019_2022%>%dplyr::select(-c("counter_2019_2022"))))
		}
	#def_frame_full_2019_2022 <- data.frame(cbind(def_frame_full_2019_2022, char_frame_2019_2022)) #Adding character frames back
	
	#Exporting the csv
	#write.csv(def_frame_full_2019_2022, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_dataframes/2019-2022/", df_nms[i], "_2019_2022_norm.csv", sep = ""))
	
}


#Extra code for just 2023

##2023
#	df_2023 <- data.frame(df_list[[i]])%>%filter(yyyy.MM.dd.HH.mm >= "2023-01-01 00:00")
#	char_frame_2023 <- df_2023%>%dplyr::select(where(is.character)) #Getting a dataframe of character variables
#	num_frame_2023 <- df_2023%>%dplyr::select(where(is.numeric))%>%dplyr::select(-c("CH4..ppm."))	 #Getting a dataframe for numerical predictors
#	num_fcnames_2023 <- colnames(num_frame) #Getting list of dataframe columns for later in the loop
#	meth_frame_2023 <- df_2023%>%dplyr::select(c("CH4..ppm.")) #Getting a dataframe for the numerical dependent
	
	#Removing variables that will not normalize (to be added back later)
#	counter_2023 <- c(linspace(1, nrow(num_frame_2023), nrow(num_frame_2023)))
#	zero_mean_df_2023 <- data.frame(counter_2023) #Create a blank dataframe to append to
#	for (j in 1:length(num_fcnames_2023)) { 
#		df_colname_2023 <- as.character(num_fcnames_2023[j])
#		df_colmean_2023 <- mean(num_frame_2023[,df_colname_2023])
#		if (df_colmean_2023 == 0) { 
#			zero_mean_df_2023[[df_colname_2023]] <- num_frame_2023[[df_colname_2023]] #Add it to a seperate dataframe so it 
#			num_frame[[df_colname_2023]] <- NULL #Remove dataframe column so it can be added back
#		} else if (is.na(df_colmean_2023)) { 
#			num_frame[[df_colname_2023]] <- NULL			
#		} else { 
			#Do nothing
#		}
#	}
	
	#Normalizing the data
#	num_frame_2023 <- num_frame_2023%>%mutate((across(everything(), num_normalizer)))
	#summary(num_frame_2023)
#	meth_frame_2023 <- meth_frame_2023%>%mutate((across(everything(), num_normalizer)))
	#summary(meth_frame_2023)
#	def_frame_full_2023 <- data.frame(cbind(num_frame_2023, meth_frame_2023))%>%select_if(~ !any(is.na(.)))
	#summary(def_frame_full_2023)
	
	#Adding back numerical data that could not be normalized
#	if (ncol(zero_mean_df_2023) != 1) { 
#		def_frame_full_2023 <- data.frame(cbind(def_frame_full_2023, zero_mean_df_2023%>%dplyr::select(-c("counter_2023"))))
#		}
	#def_frame_full_2023 <- data.frame(cbind(def_frame_full_2023, char_frame_2023)) #Adding character frames back
	
	#Esporting the csv
#	write.csv(def_frame_full_2023, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized/2023/", df_nms[i], "_2023_norm.csv", sep = ""))








