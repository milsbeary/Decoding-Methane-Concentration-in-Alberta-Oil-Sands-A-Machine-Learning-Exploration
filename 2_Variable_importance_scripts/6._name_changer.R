#CLEANS UP NAMES IN THE VARIABLE IMPORTANCE CSVS SO THEY CAN BE USED AS A BASE FOR THE VARIABLE IMPORTANCE TABLES IN THE MANUSCRIPT 
#"DECODING METHANE CONCENTRATIONS IN ALBERTA OIL SANDS: A MACHINE LEARNING EXPLORATION"

Run 1._start_up_code.R 
Run 2._data_upload_and_cleaning.R
Run 3._variable_importance.R


####UPLOADING, CLEANING, AND EXPORTING####

var_imp_path <- "/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/var_import/Dirty" #Define data path
var_imp_files <- c(list.files(path=var_imp_path, pattern="*.csv", full.names=TRUE)) #Get list of all csv files in the folder
var_imp_files #Take a look and make sure everything is there

var_imp_clean <- "/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/var_import/Clean/" #Specify path for clean data

for (e in 1:length(var_imp_files)) { 
	
	var_imp_in <- data.frame(read.csv(var_imp_files[e], check.names=FALSE))%>%dplyr::select(-c("Var.1", "df", "ScalOrNorm", "Group_type")) #uploading
	
	var_dfs <- data.frame(read.csv(var_imp_files[e], check.names=FALSE))%>%dplyr::select(c("df")) #Getting a dataframe for rownames
	
	uniq_ents <- c(unique_entryfinder(var_imp_in)) #Get a vector of unique enteries
	#uniq_ents

	clean_ents <- c((string_cleaner(uniq_ents))$clean) #Get a list of clean enteries
	#clean_ents

	numb_base_df <- data.frame(number_assigner(uniq_ents)) #Building a base dataframe
	numb_base_df$Clean_vars <- clean_ents #Assignes the clean variables to the datafame
	#numb_base_df

	new_clean_frame <- data_frame_cleaner(numb_base_df, var_imp_in) #Build new big clean
	new_clean_frame <- cbind(var_dfs, new_clean_frame) #Adding the rownames back
	colnames(new_clean_frame) <- c("Station", linspace(1, ncol(var_imp_in), ncol(var_imp_in))) #Defining neater columnnames
	
	print(new_clean_frame)
	
	export_path <- paste(var_imp_clean, substr(var_imp_files[e], 174, nchar(var_imp_files[e])), sep = '')
	#print(export_path)
	write.csv(new_clean_frame, export_path) #Write the csv of the clean file in place of the old
	
	}
















