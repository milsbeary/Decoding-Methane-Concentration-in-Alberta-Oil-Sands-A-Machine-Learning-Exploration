#EXTRACTS ALL POSSIBLE CLOUDCOVER DATA FROM DOWNLOADED NOAA DATA
#"DECODING METHANE CONCENTRATIONS IN ALBERTA OIL SANDS: A MACHINE LEARNING EXPLORATION"

#Makes sure there is no scientific notation on outputs
options(scipen=999)

#Import needed libraries
library(tidyverse)
library(lubridate)
library(data.table) #For fread and fwrite
library(dplyr)
library(stringr)


Run 1._WBEA_AIRPORT_DOWNLOADER.ipynb or make sure you have all weather data downloaded and in an accessable folder


#Get path of hourly CSV files
class_ts_path <- "/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/WBEA_Airport_Weather_Data/Hourly"
class_ts_files <- c(list.files(path=class_ts_path, pattern="*.csv", full.names=TRUE))
cl_ts_len <- length(class_ts_files)
cl_ts_len



####FINDING WHAT CSVS HAVE CLOUD COVER DATA AND WHAT TYPE####


cc_iternums <- c() #Creating a vector for itteration number 
cc_colel <- c() #Tells us what column names allign with that itteration

cc_collist <- c("AY1", "AY2", "GA1", "GA2", "GA3", "GE1", "GD1", "GD2", "GD3", "GD4", "GD5", "GD6", "GG1", "GG2", "GG3", "GG4", "GG5", "GG6", "GF1") #Gives a list of column names that contain cloudcover

for (i in 1:cl_ts_len) {
	
#print(i)
	
blank_stat_in <- data.frame(data.table::fread(class_ts_files[i]))
stat_colnames <- colnames(blank_stat_in)

	for (j in 1:length(stat_colnames)) { 
	
		colname_in <- stat_colnames[j]
	
		if (colname_in %in% cc_collist) {
	
			cc_iternums <- append(cc_iternums, i)
			cc_colel <- append(cc_colel, colname_in)
		 
		}
		
	}

}

colent_df <- data.frame(cc_iternums, cc_colel) #Puts into dataframe
colnames(colent_df) <- c("Itteration", "Colnames") #Renames columns into something easier to handle 
poss_colnames <- c(unique(colent_df$Colnames)) #Gets a list of unique colnames as a vector
colent_df #View dataframe



####BUILDING CLOUD COVER DATAFRAMES AND EXPORTING#####


new_cols <- c("A", "B", "C", "D", "E", "F", "H", "I", "J", "K", "L", "M", "N") #Defining new column names for variable split

for (i in 1:length(poss_colnames)) { 
	
	print(i)
	print(poss_colnames[i])
	
	df_subsection <- colent_df%>%filter(Colnames == poss_colnames[i]) #Gets a subsection of all data with the specificed column
	itter_nums <- c(as.numeric(df_subsection$Itteration)) #Gets a list of the itteration number, and makes sure that it is numeric
	#print(poss_colnames[i])
	#print(itter_nums)
	
	#Get first dataframe for the loop
	
	blank_stat_in <- data.frame(data.table::fread(class_ts_files[itter_nums[1]], select = c("NAME", "STATION", "DATE", "LATITUDE", "LONGITUDE", "REPORT_TYPE", poss_colnames[i])))
	
	for (j in itter_nums[2:length(itter_nums)]) { 
		
		#print(j)
	
		blank_stat_in <- rbind(blank_stat_in, data.frame(data.table::fread(class_ts_files[j], select = c("NAME", "STATION", "DATE", "LATITUDE", "LONGITUDE", "REPORT_TYPE", poss_colnames[i]))))
		
		}
		
		#summary(blank_stat_in)
		
		#Seperating and filtering
		no_blank_df <- blank_stat_in%>%filter(!is.na(.[[ncol(.)]]) & .[[ncol(.)]] != "") #Removing all blank or na rows
		str_to_count <- unlist(strsplit(as.character(no_blank_df[1, ncol(no_blank_df)]), split = "")) #Get each individual element of the string, will need this to seperate by commas 
		comma_count <- 0
		for (k in 1:length(str_to_count)) {
			
			if (str_to_count[k] == ",") {
				
				comma_count = comma_count + 1
				
			}
			
		}
		
		last_column <- names(no_blank_df)[ncol(no_blank_df)] #Getting the last column in our dataframe
		seperated_df <- no_blank_df%>%separate(last_column, new_cols[1:(comma_count+1)], sep = ',') #Seperating into what we need 
		
		#head(seperated_df)
		
		#Cleaning the data 
		
		if (poss_colnames[i] %in% c("GF1", "GD1", "GD2", "GD3", "GD4", "GD5", "GD6")) { 
			
			seperated_df_to_ex <- seperated_df%>%filter(C == 1) #Remove all cloudcover data that does not pass QCs
			
		} else if (poss_colnames[i] == "GE1") {
		
			seperated_df_to_ex <- data.frame() 
		
		} else {
			
			seperated_df_to_ex <- seperated_df%>%filter(B == 1)
			
		}
	
		
		#Exporting 
		
		if (nrow(seperated_df_to_ex) == 0) {
			
			#Do nothing
			
		} else {
		
			write.csv(seperated_df_to_ex, paste("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/WBEA_Airport_Weather_Data/CChourly/CC_CAT_", poss_colnames[i], ".csv" , sep = "")) #Exporting
			
		}
			
}



####CLEANING AND EXPORTING THE NEEDED DATA FOR THE MANUSCRIPT####


#GF_DATA

CC_CAT_GF1 <- data.frame(data.table::fread("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/WBEA_Airport_Weather_Data/CChourly/CC_CAT_GF1.csv"))%>%mutate(DATE = as_datetime(DATE), SKY_COVER_SS = A, SKY_COVER_SS_QC = C)%>%filter(DATE >= as_datetime("2019-01-01 00:00:00") & DATE < as_datetime("2024-01-01 00:00:00"))%>%dplyr::select(-c("A", "B", "C", "D", "E", "F", "H", "I", "J", "K", "L", "M", "N"))%>%filter(NAME %in% c("FORT MCMURRAY CS ALTA, CA", "FORT MCMURRAY AIRPORT, CA", "FORT MCMURRAY AWOS AIRPORT, CA"))
summary(CC_CAT_GF1)
unique(CC_CAT_GF1$NAME)

#GD_DATA

CC_CAT_GD1 <- data.frame(data.table::fread("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/WBEA_Airport_Weather_Data/CChourly/CC_CAT_GD1.csv"))%>%mutate(DATE = as_datetime(DATE), SKY_COVER_SS = A, SKY_COVER_SS_QC = C)%>%filter(DATE >= as_datetime("2019-01-01 00:00:00") & DATE < as_datetime("2024-01-01 00:00:00"))%>%dplyr::select(-c("A", "B", "C", "D", "E", "F"))%>%filter(NAME %in% c("FORT MCMURRAY CS ALTA, CA", "FORT MCMURRAY AIRPORT, CA", "FORT MCMURRAY AWOS AIRPORT, CA"))
summary(CC_CAT_GD1)
unique(CC_CAT_GD1$NAME)


CC_CAT_GD2 <- data.frame(data.table::fread("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/WBEA_Airport_Weather_Data/CChourly/CC_CAT_GD2.csv"))%>%mutate(DATE = as_datetime(DATE), SKY_COVER_SS = A, SKY_COVER_SS_QC = C)%>%filter(DATE >= as_datetime("2019-01-01 00:00:00") & DATE < as_datetime("2024-01-01 00:00:00"))%>%dplyr::select(-c("A", "B", "C", "D", "E", "F"))%>%filter(NAME %in% c("FORT MCMURRAY CS ALTA, CA", "FORT MCMURRAY AIRPORT, CA", "FORT MCMURRAY AWOS AIRPORT, CA"))
summary(CC_CAT_GD2)
unique(CC_CAT_GD2$NAME)


CC_CAT_GD3 <- data.frame(data.table::fread("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/WBEA_Airport_Weather_Data/CChourly/CC_CAT_GD3.csv"))%>%mutate(DATE = as_datetime(DATE), SKY_COVER_SS = A, SKY_COVER_SS_QC = C)%>%filter(DATE >= as_datetime("2019-01-01 00:00:00") & DATE < as_datetime("2024-01-01 00:00:00"))%>%dplyr::select(-c("A", "B", "C", "D", "E", "F"))%>%filter(NAME %in% c("FORT MCMURRAY CS ALTA, CA", "FORT MCMURRAY AIRPORT, CA", "FORT MCMURRAY AWOS AIRPORT, CA"))
summary(CC_CAT_GD3)
unique(CC_CAT_GD3$NAME)


CC_CAT_GD4 <- data.frame(data.table::fread("/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/WBEA_Airport_Weather_Data/CChourly/CC_CAT_GD4.csv"))%>%mutate(DATE = as_datetime(DATE), SKY_COVER_SS = A, SKY_COVER_SS_QC = C)%>%filter(DATE >= as_datetime("2019-01-01 00:00:00") & DATE < as_datetime("2024-01-01 00:00:00"))%>%dplyr::select(-c("A", "B", "C", "D", "E", "F"))%>%filter(NAME %in% c("FORT MCMURRAY CS ALTA, CA", "FORT MCMURRAY AIRPORT, CA", "FORT MCMURRAY AWOS AIRPORT, CA"))
summary(CC_CAT_GD4)
unique(CC_CAT_GD4$NAME)


data_list <- list(CC_CAT_GD1, CC_CAT_GD2, CC_CAT_GD3, CC_CAT_GD4, CC_CAT_GF1)
list_in <- data.frame(data_list[[1]])
for (r in 2:length(data_list)) { 
	list_in <- rbind(list_in, data.frame(data_list[[r]]))
	}

fin_GD1_df <- list_in[!duplicated(list_in$DATE), ] #Removing duplicate datetime entries
nrow(list_in) #Seeing how much is lost
nrow(fin_GD1_df)


write.csv(fin_GD1_df, "/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/WBEA_Airport_Weather_Data/CChourly/FT_MAC_SS_DAT.csv")



