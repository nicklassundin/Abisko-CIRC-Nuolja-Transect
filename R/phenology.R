##########################################################################
### Import 2024 Nuolja phenology data and then do some basic formating ###
##########################################################################
### Last modified by Pia Raker on 17 January 2025                      ###
##########################################################################


## required libraries for code ##
library(readxl);
library(data.table);
library(stringr);
library(dplyr);

#' @title read_descriptions'
#' @description columns Poles, Plot and Subplot
plots_and_subplots <- read.csv("descriptions/plots_and_subplots.csv", header = TRUE, stringsAsFactors = FALSE)

## Columns on first transform from Raw data
DEF_COLS <- c("Synonym Current","Date","Poles","Code")
## Columns on second transform from first transform
DEF_COLS_2 <- c("Synonym Current", "Year", "Subplot", "Number of Observations")
DEF_COLS_3 <- c("Synonym Current", "Year", "Plot", "Number of Observations")


## clear memory of all datasets ##
# rm(list = ls())

#' @title read_excel_allsheets 
#' @description This function reads in all sheets from an Excel spreadsheet
#' @param filename The name of the Excel file
read_excel_allsheets <- function(filename) {
	sheets <- readxl::excel_sheets(filename);
	x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, range = ("C140:ED272"),trim_ws = TRUE));
	names(x) <- sheets;
	return(x);
}

#' @title process_phenology_data
#' @description This function reads in all phenology data from the specified path
#' @param path The path to the directory containing the phenology data
#' @param dirs The directories to search for the phenology data
#' @return A data frame containing the phenology data
process_phenology_data <- function(path, dirs){
	# read only .csv files from path
	paths <- list.files(path, pattern = ".csv", full.names = TRUE, recursive = FALSE)
	paths <- paths[grepl("Plant Phenology Data/Nuolja_Data_\\d{4}.csv$", paths)]
	# combined dataframe
	combined_data <- data.frame()
	# loop through all files in the directory
	for (i in 1:length(paths)){
		# read in the data
		data <- read.csv(paths[i], header = TRUE, stringsAsFactors = FALSE)
		# add the year to the data
		data$Year <- as.numeric(str_extract(paths[i], "\\d{4}"))
		# add the data to the combined data
		colnames(data) <- c(DEF_COLS, "Year")
		combined_data <- rbind(combined_data, data)	
	}
	# mutate Poles into Subplot and Plot from the plots_and_subplots data 
	combined_data <- merge(combined_data, plots_and_subplots, by.x = "Poles", by.y = "Poles", all.x = TRUE)
	# remove the Poles column
	combined_data <- combined_data[, !(colnames(combined_data) %in% c("Poles"))]
		
	# group by "Year" and "Synonym Current" count the number of observations
	combined_data_subplot <- combined_data %>% group_by(`Synonym Current`, Year, `Subplot`) %>% summarise(n = n())
	combined_data_subplot <- combined_data_subplot[order(combined_data_subplot$Year, combined_data_subplot$`Synonym Current`),]
	colnames(combined_data_subplot) <- DEF_COLS_2
	combined_data_plot <- combined_data %>% group_by(`Synonym Current`, Year, `Plot`) %>% summarise(n = n())
	combined_data_plot <- combined_data_plot[order(combined_data_plot$Year, combined_data_plot$`Synonym Current`),]
	colnames(combined_data_plot) <- DEF_COLS_3
	# order the data by year and synonym
	closeAllConnections()
	output_path <- paths[1]
	# remove the file name from the path
	output_path <- gsub("Nuolja_Data_\\d{4}.csv", "", output_path)
	output_path <- gsub("/data/", "/out/", output_path)
	# add the new file name
	output_path <- paste(output_path, "Nuolja_Annual_Species_Observations_Subplot.csv", sep="")
	write.csv(combined_data_subplot, output_path, row.names=FALSE)
	output_path <- gsub("Nuolja_Annual_Species_Observations_Subplot.csv", "Nuolja_Annual_Species_Observations_Plot.csv", output_path)
	write.csv(combined_data_plot, output_path, row.names=FALSE)
	return(TRUE)
}

DATA_FILE_PATTERN = "^Nuolja Transect Phenology Data Entry Segments \\d+ to \\d+ \\d{4} CURRENT\\.xlsx$"

#' @title phenology_excel_to_csv 
#' @description This function reads in all sheets from an Excel spreadsheet
#' @param dir The directory where the Excel file is located
#' @param filename The name of the Excel file
#' @return A list of data frames, one for each sheet in the Excel file
phenology_excel_to_csv <- function(dir){
	# retreive file in directory matching the filename with regex pattern with out year
	file <- list.files(dir, pattern = DATA_FILE_PATTERN)
	print(file)
	if (length(file) == 0){
		print("No file found")
		return(NULL)
	}
	Nuolja.Data <- read_excel_allsheets(paste(dir, file, sep="/"))

	## split data in the single column into multiple columns by the comma delimiter ##
	Nuolja.Data <- setDT(Nuolja.Data)[, list(var = unlist(.SD))][, tstrsplit(var, ",")]


	## delete all null records, e.g. all data in a record is NA ##
	Nuolja.Data <- Nuolja.Data[rowSums(is.na(Nuolja.Data))!=ncol(Nuolja.Data), ]

	## assign names to columns ##
	columns <- c("Date","Subplot","Species")
	for (i in 1:(ncol(Nuolja.Data)-3)){
		columns <- c(columns, paste("Code.", i, sep=""))
	}
	colnames(Nuolja.Data) <- columns 

	## convert character to date for Date column ##
	Nuolja.Data$Date <- as.Date(Nuolja.Data$Date, format="%d/%m/%Y")


	## remove whitespace at end of species names ##
	Nuolja.Data$Species <- trimws(Nuolja.Data$Species, which = c("right"))

	## extract only the species name, not the variety or subspecies ##
	Nuolja.Data$SpeciesName <- word(Nuolja.Data$Species, start=1, end =2, sep=fixed(" "))

	## sort data set by date, Subplot and species ##
	Nuolja.Data <- Nuolja.Data[with(Nuolja.Data, order(Date, Subplot, Species)), ]
	
	# take the first none NA value from the code columns exc
	Nuolja.Data$Code <- apply(Nuolja.Data[,4:(ncol(Nuolja.Data)-1)], 1, function(x) x[!is.na(x)][1])
	
	## remove records with no observations
	Nuolja.Data <- Nuolja.Data[!is.na(Nuolja.Data$Code),]
		
	## sort data set by date, Subplot and species ##
	Nuolja.Data <- cbind(Nuolja.Data[,(ncol(Nuolja.Data)-1)], Nuolja.Data[,1:2], Nuolja.Data[,ncol(Nuolja.Data)])

	## Rename fields to match Nuolja Project MS Access Database ##
	colnames(Nuolja.Data) <- DEF_COLS


	##Create text file of the newly formatted dataset ##
	print(dir)
	# TODO Compare to the original results
	# write.csv(Nuolja.Data, "Nuolja.Data.csv", row.names=FALSE)
}

