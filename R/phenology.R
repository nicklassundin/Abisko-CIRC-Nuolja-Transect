##########################################################################
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
	output_path <- gsub("data/", "out/", output_path)
	# add the new file name
	output_path <- paste(output_path, filename, sep="/")
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

read_phenology_data <- function(dir, all=FALSE) {
	# read only .csv files from path
	paths <- list.files(dir, pattern = ".csv", full.names = TRUE, recursive = FALSE)
	answer = NA;
	while(!all){
		for(i in 1:length(paths)){
			print(paste(i, ") ", paths[i], sep=""))
		}
		print("Select the files to process (separated by ; or ,; Alt. n-m for interval):")
		answer = readLines(file("stdin"), n = 1)
		# split on ; and ,
		# if contain -
		if (grepl("-", answer)){
			# split on -
			answer = unlist(strsplit(answer, "-"))
			# convert to numeric to if 2 convert to interval a:b
			answer = as.numeric(answer[1]):as.numeric(answer[2])
		}else{
			answer = unlist(strsplit(answer, "[;,]"))
		}
		answer = paths[as.numeric(answer)]
		if(answer %in% paths){
			paths <- answer
			print(paste("You selected:", paths))
			break;
		} else {
			print("Invalid selection. Please try again.");
		}
	}
	paths <- paths[grepl("Nuolja_Data_\\d{4}.csv$", paths)]

	combined_data <- bind_rows(lapply(paths, function(path) {

						  # combined_data <- bind_rows(lapply(paths, function(path) {
						  # if (!file.exists(path)){
						  # return(NULL)
						  # }
						  data <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
						  data$Year <- as.numeric(str_extract(path, "\\d{4}"))
						  colnames(data) <- c(DEF_COLS, "Year")
						  return(data)
}))
	return(list(data=combined_data, paths=paths))
}

process_phenology_data <- function(input, dirs){
	# read only .csv files from path
	# filter NA values
	dirs <- dirs[!is.na(dirs)]
	combined_data <- input$data
	paths <- input$paths
	create_file_structure("phen.log")
	validator = PhenologyValidator$new()
	# create log files and save old log in backup.*.log
	valid = lapply(paths, (function(x) {
		return(validateFile(x, validator=validator, log_file="phen.log", head=TRUE))
	}))
	# length of valid TRUE
	# keep only valid rows in combined_data
	combined_data <- combined_data[unlist(valid),]
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
	output_path <- get_output_path(dirs[1], "Nuolja_Annual_Species_Observations.csv") 
	write.csv(observ_data, output_path, row.names=FALSE)
	# calculate first observation date for each year 
	first_observation <- combined_data %>%
		group_by(`Synonym Current`, Year, Code, Poles) %>%
		summarise(`First Observation Date` = min(Date), `Last Observation Date` = max(Date), .groups = "drop")
	colnames(first_observation) <- c("Synonym Current", "Year", "Code", "Poles", "First Observation Date", "Last Observation Date")
	# output_path replace the file name with the new file name
	output_path <- get_output_path(dirs[1], "Nuolja_First_Last_Observation_Date.csv")
	write.csv(first_observation, output_path, row.names=FALSE)

	# create number of observations per year
	# species year subplot numober of fielddays
	# add day of the year
	combined_data$DoY <- as.numeric(strftime(combined_data$Date, "%j"))
	# filter out unique DoY
	number_obs <- combined_data %>% group_by(`Synonym Current`, Year, `Poles`, DoY) %>% summarise(n = 1, .groups = "drop")
	number_obs <- number_obs %>% group_by(`Synonym Current`, Year, `Poles`) %>% summarise(`Number of Observations` = n(), .groups = "drop")

	# output_path replace the file name with the new file name
	output_path <- get_output_path(dirs[1], "Nuolja_Annual_Species_Days_Observed.csv")
	write.csv(number_obs, output_path, row.names=FALSE)
	print("Completed processing phenology data")
	return(TRUE)
}

