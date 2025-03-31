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
		group_by(`Synonym Current`, Year, Code) %>%
		summarise(`First Observation Date` = min(Date), `Last Observation Date` = max(Date), .groups = "drop") %>%
		left_join(
			  combined_data %>%
				  group_by(`Synonym Current`, Year, Code) %>%
				  slice_min(Date, with_ties = FALSE) %>%
				  select(`Synonym Current`, Year, Code, `Poles First` = Poles),
			  by = c("Synonym Current", "Year", "Code")
		) %>%
		left_join(
			  combined_data %>%
				  group_by(`Synonym Current`, Year, Code) %>%
				  slice_max(Date, with_ties = FALSE) %>%
				  select(`Synonym Current`, Year, Code, `Poles Last` = Poles),
		)
	colnames(first_observation) <- c("Synonym Current", "Year", "Code", "First Observation Date", "Last Observation Date", "Poles First", "Poles Last")
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

#' @title phenology_excel_to_csv 
#' @description This function reads in all sheets from an Excel spreadsheet
#' @param dir The directory where the Excel file is located
#' @param filename The name of the Excel file
#' @return A list of data frames, one for each sheet in the Excel file
#phenology_excel_to_csv <- function(dir){
#	# retreive file in directory matching the filename with regex pattern with out year
#	file <- list.files(dir, pattern = DATA_FILE_PATTERN)
#	print(file)
#	if (length(file) == 0){
#		print("No file found")
#		return(NULL)
#	}
#	Nuolja.Data <- read_excel_allsheets(paste(dir, file, sep="/"))

#	## split data in the single column into multiple columns by the comma delimiter ##
#	Nuolja.Data <- setDT(Nuolja.Data)[, list(var = unlist(.SD))][, tstrsplit(var, ",")]


#	## delete all null records, e.g. all data in a record is NA ##
#	Nuolja.Data <- Nuolja.Data[rowSums(is.na(Nuolja.Data))!=ncol(Nuolja.Data), ]

#	## assign names to columns ##
#	columns <- c("Date","Subplot","Species")
#	for (i in 1:(ncol(Nuolja.Data)-3)){
#		columns <- c(columns, paste("Code.", i, sep=""))
#	}
#	colnames(Nuolja.Data) <- columns 

#	## convert character to date for Date column ##
#	Nuolja.Data$Date <- as.Date(Nuolja.Data$Date, format="%d/%m/%Y")


#	## remove whitespace at end of species names ##
#	Nuolja.Data$Species <- trimws(Nuolja.Data$Species, which = c("right"))

#	## extract only the species name, not the variety or subspecies ##
#	Nuolja.Data$SpeciesName <- word(Nuolja.Data$Species, start=1, end =2, sep=fixed(" "))

#	## sort data set by date, Subplot and species ##
#	Nuolja.Data <- Nuolja.Data[with(Nuolja.Data, order(Date, Subplot, Species)), ]
	
#	# take the first none NA value from the code columns exc
#	Nuolja.Data$Code <- apply(Nuolja.Data[,4:(ncol(Nuolja.Data)-1)], 1, function(x) x[!is.na(x)][1])
	
#	## remove records with no observations
#	Nuolja.Data <- Nuolja.Data[!is.na(Nuolja.Data$Code),]
		
#	## sort data set by date, Subplot and species ##
#	Nuolja.Data <- cbind(Nuolja.Data[,(ncol(Nuolja.Data)-1)], Nuolja.Data[,1:2], Nuolja.Data[,ncol(Nuolja.Data)])

#	## Rename fields to match Nuolja Project MS Access Database ##
#	colnames(Nuolja.Data) <- DEF_COLS


#	##Create text file of the newly formatted dataset ##
#	print(dir)
#	# TODO Compare to the original results
#	# write.csv(Nuolja.Data, "Nuolja.Data.csv", row.names=FALSE)
#}

