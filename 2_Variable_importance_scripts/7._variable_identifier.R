#BUILDS A BASE FOR THE VARIABLE INDEX TABLE IN THE SUPPLIMENT
#"DECODING METHANE CONCENTRATIONS IN ALBERTA OIL SANDS: A MACHINE LEARNING EXPLORATION"


####UPLOADING ALL REQUIRED DATA####

var_id_path <- "/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_dataframes/2019-2022" #Define data path
var_id_files <- c(list.files(path=var_id_path, pattern="*.csv", full.names=TRUE)) #Get list of all csv files in the folder
var_id_files 


#####FINDING ALL VARIABLE NAMES#####

uniq_names <- c() #The vector that will contain all unique variable names

for (i in 1:length(var_id_files)) { 
	
	var_id_in <- data.frame(read.csv(var_id_files[i], check.names=FALSE)) #Uploading dataframe
	var_id_cnames <- colnames(var_id_in) #Extracting columnnames
	uniq_names <- unique(append(uniq_names, var_id_cnames)) #Appending to the variable names vector and making sure it is unique
	
}

	
#Getting the list of clean names

clean_names <- data.frame(string_cleaner(uniq_names)$clean)


#Building the base dataframe

base_df <- number_assigner(uniq_names)


#Building the final frame

base_df_2 <- cbind(clean_names, base_df) #Combining the two dataframes
colnames(base_df_2) <- c("Abriviation", "Variable", "Unit") #Setting the column names to what we want
base_df_2 #Viewing 


#Exporting

#write.csv(base_df_2, "/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Variable_index_base/Variable_names.csv")







