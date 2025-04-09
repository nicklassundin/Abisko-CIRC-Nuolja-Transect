########################################## %>%################################
### Import 2024 Nuolja phenology data and then do some basic formating ###
##########################################################################
### Last modified by Pia Raker on 17 January 2025                      ###
##########################################################################


## required libraries for code ##
# library(readxl);
library(data.table);
library(stringr);
library(dplyr);
library(tidyr);

library(openxlsx)



#' @title read_descriptions'
#' @description columns Poles, Plot and Subplot
plots_and_subplots <- read.csv("descriptions/plots_and_subplots.csv", header = TRUE, stringsAsFactors = FALSE)

#' @title phenology_codes
#' @description This function reads in the phenology codes from the given path and directories
pheno_codes <- read.csv("descriptions/phenology_codes.csv", header = TRUE, stringsAsFactors = FALSE)

## Columns on first transform from Raw data
DEF_COLS <- c("Synonym Current","Date","Poles","Code")
## Columns on second transform from first transform
DEF_COLS_2 <- c("Synonym Current", "Year", "Poles","Code", "Number of Observations")


#' @title get output path
#' @description This function returns the output path for the given input path
#' @param path The input path
#' @return The output path
get_output_path <- function(path, filename){
	# remove the file name from the path
	output_path <- gsub("Nuolja_Data_\\d{4}.csv", "", path)
	output_path <- gsub("/data/", "/out/", output_path)
	# add the new file name
	output_path <- paste(output_path, filename, sep="")
	return(output_path)
}

## clear memory of all datasets ##
# rm(list = ls())

#' @title read_excel_allsheets 
#' @description This function reads in all sheets from an Excel spreadsheet
#' @param filename The name of the Excel file
# read_excel_allsheets <- function(filename) {
# 	sheets <- readxl::excel_sheets(filename);
# 	x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, range = ("C140:ED272"),trim_ws = TRUE));
# 	names(x) <- sheets;
# 	return(x);
# }

#' @title process_phenology_data
#' @description This function reads in all phenology data from the given path and directories and processes it into two files; Observation by Year, Species, Code and Subplot; Second First observation by Year, Species and Code
#' @param path The path to the directory containing the phenology data
#' @param dirs The directories to search for the phenology data
#' @return A data frame containing the phenology data
process_phenology_data <- function(path, dirs){
	# read only .csv files from path
	paths <- list.files(path, pattern = ".csv", full.names = TRUE, recursive = FALSE)
	# filter NA values
	# paths <- paths[!is.na(paths)]
	paths <- paths[grepl("Plant Phenology Data/Nuolja_Data_\\d{4}.csv$", paths)]

	# validate files TODO
	# valid = lapply(paths, (function(x) {
	# return(validateFile(x, PATTERNS=PHENO_PATTERNS, log_file="phen.log", head=TRUE))
	# }))
	# print(valid[1:10])
	valid = paths

	# paths <- paths[unlist(valid)]
	# filter paths that are valid
	combined_data <- bind_rows(lapply(valid, function(path) {

						  # combined_data <- bind_rows(lapply(paths, function(path) {
						  # if (!file.exists(path)){
						  # return(NULL)
						  # }
						  data <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
						  data$Year <- as.numeric(str_extract(path, "\\d{4}"))
						  colnames(data) <- c(DEF_COLS, "Year")
						  return(data)
}))
	survey_tables(combined_data)
	return(TRUE)

	# group by "Year" and "Synonym Current" count the number of observations
	observ_data <- combined_data %>% group_by(`Synonym Current`, Year, `Poles`, Code) %>% summarise(n = n(), .groups = "drop")

	observ_data <- observ_data[order(observ_data$Year, observ_data$`Synonym Current`),]
	colnames(observ_data) <- DEF_COLS_2
	# for each Synonym Current
	obser_data_complete <- observ_data %>% complete(Year, `Synonym Current`, Poles, Code = pheno_codes$codes)
	observ_data <- rbind(observ_data, obser_data_complete)
	# remove Synonym Current, year, poles, where number of observations are NA

	# sort column by synonym and year
	observ_data <- observ_data[order(observ_data$`Synonym Current`, observ_data$Year, observ_data$Poles, observ_data$Code),]

	# order the data by year and synonym
	output_path <- get_output_path(paths[1], "Nuolja_Annual_Species_Observations.csv") 
	write.csv(observ_data, output_path, row.names=FALSE)
	# calculate first observation date for each year 
	first_observation <- combined_data %>%
		group_by(`Synonym Current`, Year, Code, Poles) %>%
		summarise(`First Observation Date` = min(Date), `Last Observation Date` = max(Date), .groups = "drop")
	colnames(first_observation) <- c("Synonym Current", "Year", "Code", "Poles", "First Observation Date", "Last Observation Date")
	# output_path replace the file name with the new file name
	output_path <- get_output_path(paths[1], "Nuolja_First_Last_Observation_Date.csv")
	write.csv(first_observation, output_path, row.names=FALSE)

	# create number of observations per year
	# species year subplot numober of fielddays
	# add day of the year
	combined_data$DoY <- as.numeric(strftime(combined_data$Date, "%j"))
	# filter out unique DoY
	number_obs <- combined_data %>% group_by(`Synonym Current`, Year, `Poles`, DoY) %>% summarise(n = 1, .groups = "drop")
	number_obs <- number_obs %>% group_by(`Synonym Current`, Year, `Poles`) %>% summarise(`Number of Observations` = n(), .groups = "drop")

	# output_path replace the file name with the new file name
	output_path <- get_output_path(paths[1], "Nuolja_Annual_Species_Days_Observed.csv")
	write.csv(number_obs, output_path, row.names=FALSE)

	return(TRUE)
}

DATA_FILE_PATTERN = "^Nuolja Transect Phenology Data Entry Segments \\d+ to \\d+ \\d{4} CURRENT\\.xlsx$"

datasheet_info <- normalizePath('descriptions/Nuolja\ Master\ Documents/Nuolja_Phenology_Datasheet_Information.xlsx')
species_errors <- normalizePath('descriptions/Nuolja\ Master\ Documents/Nuolja\ Species\ Errors\ and\ Observation\ Notes\ CURRENT.xlsx')


survey_tables <- function(df){
	
	species_list <- df %>% 
		# group_by(`Synonym Current`, Poles, Year) %>% 
		distinct(`Synonym Current`, Poles) %>%
		count(`Synonym Current`, Poles, year, name = "n")

	print(species_list)
	# Observation Error
	species_errors <- read.xlsx(species_errors, sheet = 1, colNames = TRUE)
	# remove columns
	species_errors <- species_errors[,c(1,2,3,4,5,7,8,9)]
	# print(species_errors[1:10,])
	# filter based on species errors 'Species Error (Y/N)' is Y
	species_list <- species_list %>%
		left_join(species_errors, 
			  by = c("Synonym Current" = "Observed.species", "Year" = "Year")) 
	
	mask = !is.na(species_list[,6]);
	species_list[mask,]$`Synonym Current` = species_list[mask,]$`Corrected.name`
	species_list <- species_list %>%
		mutate(`Field Filter` = (!is.na(`Corrected.name`)) | (`Species.Error.(Y/N)` == "N") &
					 (`single.date.observation.(Y/N)` == "N") &
				 (`High.confidence.of.correct.identification.on.species.level.(Y/N)` == "Y"))

	species_list <- species_list %>%
		filter(`Field Filter` == TRUE)
	
	print(species_list)
	# filter if present years TODO
	# species_list <- df %>% group_by(`Synonym Current`, Poles) %>% summarise(n = n(), .groups = "drop") %>%
	print(species_list)

	
	return(TRUE)

	species_list <- species_list[order(species_list$Poles),]
	# print(species_list)
	species_list <- species_list %>%
		mutate(is_present = TRUE) %>%
		distinct(`Synonym Current`,  .keep_all = TRUE) %>%
		pivot_wider(
			    names_from = Poles, 
			    values_from = is_present, 
			    values_fill = list(is_present = FALSE)
			    );

	# sort by synonym
	species_list <- species_list[order(species_list$`Synonym Current`),]

	
	# read in the datasheet information
	datasheet_info <- read.xlsx(datasheet_info, sheet = 1, colNames = TRUE)
	# print(datasheet_info[1:10,])	
	# print(datasheet_info[1:10,])	
	species_list <- species_list %>% left_join(datasheet_info, by = c("Synonym Current" = "Species")) %>%
		mutate(`Synonym Current` = if_else(!is.na(W) & !is.na(WG) & (W == "Y") & (WG == "Y"),
						   paste0(`Synonym Current`, " (W WG)"),
						   `Synonym Current`)) %>%
	mutate(`Synonym Current` = if_else(!is.na(W) & (W == "Y") & (WG != "Y" | is.na(WG)),
					   paste0(`Synonym Current`, " (W)"),
					   `Synonym Current`)) %>%
	mutate(`Synonym Current` = if_else(!is.na(WG) & WG == "Y" & (W != "Y" | is.na(W)),
					   paste0(`Synonym Current`, " (WG)"),
					   `Synonym Current`))
	# select(-W) %>% select(-WG)
	# print(species_list)


	# list <- list %>%
	# 	filter(str_match(Poles, "^(\\d{2}) to (\\d{2})")[,2] %>%
	# 	       as.integer() + 1 ==
	# 	       str_match(Poles, "^(\\d{2}) to (\\d{2})")[,3] %>%
	# 	       as.integer())
}

