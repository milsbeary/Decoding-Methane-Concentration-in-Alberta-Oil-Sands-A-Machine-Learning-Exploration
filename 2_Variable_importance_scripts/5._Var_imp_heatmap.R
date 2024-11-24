#BUILDS HEATMAP OF PURE VARIABLE IMPORTANCE (NOT USED IN MANUSCRIPT)
#"DECODING METHANE CONCENTRATIONS IN ALBERTA OIL SANDS: A MACHINE LEARNING EXPLORATION"

Run 1._start_up_code.R 
Run 2._data_upload_and_cleaning.R
Run 3._variable_importance.R



####BUILDING HEATMAP####

#Uploading dataframe for heatmap

var_imp_path <- "/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Data/Normalized_variable_importance/Pure/pure_variable_importance.csv"
var_imp_in <- data.frame(read.csv(var_imp_path, check.names=FALSE))%>%dplyr::select(-c("Var.1", "df", "ScalOrNorm", "Group_type")) #Importing dataframe and removing what we do not need
var_imp_rownames <- (data.frame(read.csv(var_imp_path, check.names=FALSE))%>%dplyr::select(c("df")))$df #Extracting the dataframe column for the rownames


#Resetting figure rownames and column names

fig_rownames <- c("Anzac", "Athabasca Valley", "Bertha Ganter", "Buffalo Viewpoint", "Conklin", "Janvier", "Lower Camp", "Manix", "Mildred Lake", "Patricia McInnis", "Stacked (with gas)", "Stacked (without gas)", "Stoney Mountain")
rownames(var_imp_in) <- fig_rownames 
fig_colnames <- as.character(linspace(1, ncol(var_imp_in), ncol(var_imp_in)))
colnames(var_imp_in) <- fig_colnames
var_imp_in #Viewing dataset


#Assigning each element a number

var_imp_els <- unique_entryfinder(var_imp_in)
print(var_imp_els)


#Assigning each element a number

numb_assign <- number_assigner(var_imp_els)
print(numb_assign)


#Creating a dataframe of assiged numbers

number_map <- assign_back(numb_assign, var_imp_in)
colnames(number_map) <- fig_colnames #Renaming because it doesnt stick
print(number_map)


#Building heatmap dataframe

number_map_long <- number_map #Doing this so we can keep the OG after the change
number_map_long$Station <- rownames(number_map) #Making sure we have the station identifier 
number_map_long <- pivot_longer(number_map_long, colnames(number_map)) #Putting the data into long format 
number_map_long$name <- factor(number_map_long$name, levels = linspace(1, ncol(var_imp_in), ncol(var_imp_in))) # Order the Variables numerically
number_map_long$Station <- factor(number_map_long$Station, levels = sort(unique(number_map_long$Station), decreasing = TRUE)) # Order the Stations alphabetically and start from the top of the axis
number_map_long$value <- factor(number_map_long$value) #Redundent but doesnt work without
value_lables <- setNames(numb_assign$Variables, numb_assign$Count) #Assign the variable names to the count in a plottable format
number_map_long$value <- as.factor(value_lables[as.character(number_map_long$value)]) #Assigns in the dataframe as a factor
print(number_map_long, n=195) #View our plot


#Build variable heatmap with ggplot

colors <- colorRampPalette(brewer.pal(11, "Spectral"))(28) #Generate a color palette with 28 distinct colors
gasmoo <- ggplot(number_map_long, aes(x = name, y = Station, fill = value)) +
  geom_tile(color = "black", linewidth = 0.3) +
  scale_fill_manual(values = setNames(colors, levels(number_map_long$value)), name = "Variable") +
  labs(x = "Rank", y = "Station") +
  guides(fill = guide_legend(ncol = 1)) + 
  theme_minimal() #+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for better visibility
gasmoo #View 


#Save figure

#save_pathbase <- "/Users/milsbeary/Desktop/Miles NSERC Alliance Missions /Air Quality Monotoring of Polutents/Land Station/FINAL_STUDY_DS/Figures/"
#ggsave(paste(save_pathbase, "pure_heatmap.jpeg", sep = ""), plot = gasmoo)



