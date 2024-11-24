#START UP CODE FOR VARIABLE IMPORTANCE 
#"DECODING METHANE CONCENTRATIONS IN ALBERTA OIL SANDS: A MACHINE LEARNING EXPLORATION"


#Delete previous information stored 
rm(list=ls(all=T))

#Makes sure there is no scientific notation on outputs
options(scipen=999)

#Loading all needed packages
library(tidyverse)
library(lubridate)
library(data.table) 
library(pracma)
library(ggplot2)
library(stringr)
library(tidyr)
library(dplyr)
library(RColorBrewer) 

#Note that if you do not have the packages, you can install them to your computer using install.packages('package_name')



####DEFINING NEEDED FUNCTIONS####


#Variable importance (This function was adapted from: https://github.com/damariszurell/SSDM-JSDM/blob/master/Univariate_variable_importance_blockCV_select07.r)
#pred_names: List or vector of predictor names 
#response_name: List or vector of response names
#data: Input dataframe 
#threshold: Set threshold for coliniarity
select07_Logit <- function(pred_names, response_name, data, threshold=thre){
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


#Generating interactions
#data_f: Dataframe input
#squares: The vector of exponential values you want to use
#square_names: The vector of exponential values names you want to use
column_interacter <- function(data_f, squares, squares_names) {
str_c_names <- colnames(data_f) 

count <- c()

for (i in 1:length(str_c_names)) { 
	
	for (j in 1:length(str_c_names)) { 	
		
		if (i <= j) {  #This is a symetric matrix so dont need to calculate across the diagonal
			
			} else { 
				
				for (k in 1:length(squares))	 { 
					
					for (l in 1:length(squares)) { 
						
						count <- append(count, 1)
						#print(length(count))
						
						col_one <- str_c_names[i]
						col_two <- str_c_names[j]
						new_cname <- paste(col_one, "_", squares_names[k], "_and_", col_two, "_", squares_names[l], sep = "")
						new_cname = str_squish(new_cname) #Removing extra whitespaces
						new_col <- c((data_f[,col_one])^squares[k])*c((data_f[,col_two])^squares[l])
					
						data_f[[new_cname]] <- new_col
						
						
						}
					
					}							
								
				}
	
		}
	
	}
return(data_f) 

} 


#Generating variable transformation
#data_f: Input dataframe 
#trans: Vector of transformations you want to use
#trans_names: Vector of transformation values names you want to use
column_transformer <- function(data_f, trans, trans_names) { 
	
	str_c_names <- colnames(data_f) 
	
	for (i in 1:length(str_c_names)) { 
		
		for (j in 1:length(trans)) {
		
			col_one <- str_c_names[i]
		
			new_cname <- paste(col_one, "_", trans_names[j], sep="")
		
			new_col <- c((data_f[,col_one])^trans[j])
		
			data_f[[new_cname]] <- new_col
		
			}
		
		}
	
return(data_f)
	
}


#For normalizing dataframe column
#x: the value of the dataframe column
num_normalizer <- function(x) { 
	
	normed_x <- (x-min(x))/(max(x)-min(x))
	
	return(normed_x)
	
}


#Find all of the unique entries in a dataframe and export as a vector
#df: Dataframe in 
unique_entryfinder <- function(df) {
	
	entry_vec <- c() #Generates an empty vector to append to
	
	for (i in 1:nrow(df)) { 
	
		for (j in 1:ncol(df)) { 
			
			entry_vec <- append(entry_vec, as.character(df[i, j])) #Appends the entry to the big vector
			
			}
			
		}
		
	return(c(unique(entry_vec))) #Retunrs a vector of unique elements
	
} 


#Assignes a number to a unique list of vectors, outputs a dataframe containing the orderd pairs
#Vec: The unique list of vectors
number_assigner <- function(vec) { 
	
	matrix_cols <- c("Variables", "Count") #Generate colnames for our matrix
	assign_matrix <- data.frame(matrix(nrow = length(vec), ncol = 2)) #Generate a dataframe to append to
	colnames(assign_matrix) <- matrix_cols #Set the generated column names as the colnames of the matrix
	numb_to_assign = 1 #Sets the first number to assign 

	for (i in 1:length(vec)) { 
		
		assign_matrix[i, "Variables"] <- vec[i] #Assigning variable name
		assign_matrix[i, "Count"] <- numb_to_assign #Assigning number to the variable name
		numb_to_assign = numb_to_assign + 1 #Adding one for the next assignment
		
		}
		
	return(data.frame(assign_matrix)) #Retuning the dataframe or orderd pairs
	
}


#Takes each string and puts it into the needed form for the manuscript
#str_vec: A vector of strings you want to transform
#Output is a list that contains a clean vector of variable names and an unclean vector of variable name units
string_cleaner <- function(str_vec) { 
	
	clean_vec <- c() #The vector we will be appending the final strings too
	units <- c() #The vector we will be appending the units too
	
	for (i in 1:length(str_vec)) { 
		str_to_cl <- str_vec[i] #Extract string that we want to clean from the vector
		#See if this string has a _ for group identification
		dash_id <- 0
		if (is.na(str_to_cl)) { 
			str_to_cl <- "NA" #Put NA into usable format
			} else {
			for (j in 1:nchar(str_to_cl)) {
			str_eleone <- substr(str_to_cl, j, j) #Get the substing element
			if (str_eleone == "_") { 
				dash_id <- 1
				break #Break if we have a dash
				}
			}
		}
		
		#Fixing exponentials and "ands"
		if (dash_id <- 1) { 
			ssplit_str <- c(strsplit(str_to_cl, "_")[[1]])
			if ("exp" %in% ssplit_str) { 
				for (k in 1:length(ssplit_str)) { 
					if (ssplit_str[k] == "and") {  
						ssplit_str[k] = "*"
						} else if (ssplit_str[k] == "exp") { 
						ssplit_str[k] = "^"	
						} else if (ssplit_str[k] == "one") { 	
						ssplit_str[k] = "1" 	
						} else if (ssplit_str[k] == "two") { 
						ssplit_str[k] = "2" 	
						} else if (ssplit_str[k] == "neg") {
						ssplit_str[k] = "-"
						}
					}
		       }		
	      }
	    
	    #Removing substrings we do not need
	   	for (k in 1:length(ssplit_str)) { 
	   		if (ssplit_str[k] == "2..mm." | ssplit_str[k] == "2..C." | ssplit_str[k] == "2..mB." | ssplit_str[k] == "16..umol.m.2.s.1." | ssplit_str[k] == "29..W.m2.") {
	   			units <- append(units, ssplit_str[k]) #Append these to units
	   			ssplit_str[k] = "" #Remove it from main strng
	   		}
	   	}
	   	
	   	#Splitting further to remove mores substring we do not need
	  	further_split <- strsplit(ssplit_str, "\\.\\.") #Split by ".."
	   	new_ind_stvec <- c() #New vector to append what we need to 
	   	for (w in 1:length(further_split)) {
	   	fur_len <- length(further_split[[w]])
	   		if (fur_len == 0) { 
	   			#Do nothing
	   			} else { 
	   			for (m in 1:fur_len) { 
	   				str_anal <- 	further_split[[w]][m]
	   				if (is.na(str_anal)) { 
	   				#Do nothing as it is a blank entrty
	   				} else if (str_anal == "km.h." | str_anal == "deg." | str_anal == "of.range." | str_anal == "ppb." | str_anal == "ug.m3." | str_anal == "C." | str_anal == "ppm.") {
	   				units <- append(units, str_anal) #Append these to units	
	   				} else if (str_anal == "") {
	   				#Do nothing as we dont need to append a blank entry		
	   				} else { 
	   				new_ind_stvec <- append(	new_ind_stvec, str_anal)
	   				}
	   			}	
	   		}
	   	}
	   
	   #Cleaning up some of the words we kept
	    for (y in 1:length(new_ind_stvec)) { 
	    	el_in_q <- new_ind_stvec[y]
	    	if (el_in_q == "STD") { 
	    		new_ind_stvec[y] <- "_SD"
	    		} else if (el_in_q == "diluent") { 
	    		new_ind_stvec[y] <- "DIL"	
	    		} else if (el_in_q == "cloud") { 
	    		new_ind_stvec[y] <- "CLD"
	    		} else if (el_in_q ==  "fraction") { 	
	    		new_ind_stvec[y] <- "_FRA"	
	    		} else if (el_in_q == "COVER") { 
	    		new_ind_stvec[y] <- "_CVR_"	
	    		} else if (el_in_q == "SR.O3") { 	
	    		new_ind_stvec[y] == "SRO3"	
	    		} else if (el_in_q == "GRW.m2.") { 	
	    		new_ind_stvec[y] == "GRW"	
	    		}
	   	}
	   
	   #Getting rid of exponentials where they are not needed
	   indexer <- 0
	   for (x in 1:length(new_ind_stvec)) { 
	   		el_in_q2 <- new_ind_stvec[x]
	   		if (el_in_q2 == "^") { 	
	   			indexer = 1			
	   			} else if (indexer == 1 & el_in_q2 == 1) { 
	   			new_ind_stvec[x] = ""
	   			new_ind_stvec[x-1] = ""
	   			indexer = 0
	   			} else {	
	   			indexer = 0	
	   			}
	   	}
	  
	  #Putting the cleaned string where we want it 
	  new_str <-  paste(new_ind_stvec, collapse = "") #Colapsing the string 
	  clean_vec <- append(clean_vec, new_str)
	   
	   }
	   
	   #Dealing with units
	   uniq_uns <- unique(units)
	   
	  #Returning what we need
	  return(list(clean = clean_vec, unit = uniq_uns))
	   	
}

#Using the clean variable names, we replace the old dataframe names
#df_ind: The dataframe with the individual clean and dirty variables
#dd_to_clean: The dataframe to replace everything with
data_frame_cleaner <- function(df_ind, df_to_clean) { 
	
	for (i in 1:nrow(df_ind)) { 
		
		for (j in 1:nrow(df_to_clean)) { 
			
			for (k in 1:ncol(df_to_clean)) { 
				
				if (is.na(df_to_clean[j, k])) { 
					
					if (is.na(df_ind[i, "Variables"])) {
						
						df_to_clean[j, k] = df_ind[i, "Clean_vars"]
						
						}
					
					} else if (is.na(df_ind[i, "Variables"])) { 
						
						#Do nothing as captured in the first case
						
					} else if (df_ind[i, "Variables"] == df_to_clean[j, k]) {
					
					df_to_clean[j, k] = df_ind[i, "Clean_vars"] #Replace
					
					}
				
				}
			
			}	
			
		}
		
	return(data.frame(df_to_clean))
	
}


#Creates a numerical dataframe baised off of the numbers we have assigned 
#df_assign: Is the dataframe of assigned numbers and variables from number_assigner
#df_assign: Is the original dataframe imported that we want to make a heat map out of
assign_back <- function(df_assign, df_manipulate) { 
		
	df_nummap <- data.frame(matrix(nrow = nrow(df_manipulate), ncol = ncol(df_manipulate))) #Creates a blank dataframe for the number to be appendend too
	colnames(df_nummap) <- colnames(df_manipulate) #Giving the new one the same columnnames as the old
	rownames(df_nummap) <- rownames(df_manipulate) #Doing the same for the rownames
	
	for (i in 1:nrow(df_manipulate)) { 
		
		for (j in 1:ncol(df_manipulate)) { 
			
			entry_toa <- df_manipulate[i, j] #Getting the entry we need the number for
			
			for (k in 1:nrow(df_assign)) { 
				
				row_entry <- df_assign[k, "Variables"] #Looking at the row enteries
				
				if (is.na(row_entry)) { #Dealing with Rs inability to handle NAs
					
					if (is.na(entry_toa)) { 
												
						df_nummap[i, j] <- df_assign[k, "Count"] #Append the NA number
						
						break #Dont need any more itterations so break the lool
						
					} else { 
						
						#No nothing as they do not match
						
						}
					
				} else { 
					
					if (is.na(entry_toa)) { 
						
						#Do nothing as they are not both NA
						
					} else if (row_entry == entry_toa) { 
						
						df_nummap[i, j] <- df_assign[k , "Count"] #Append the corrisponding number
						
						break #Dont need anymore itteration so break the loop
						
					} else {
						
						#Do nothing as they do not match
						
						}	
					
					}
							
				} 
			
			}
		
		}
		
	return(data.frame(df_nummap)) #Returns the new numbermap as a dataframe	
		
}








