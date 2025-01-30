##########################################################################
### Import 2024 Nuolja phenology data and then do some basic formating ###
##########################################################################
### Last modified by Pia Raker on 17 January 2025                      ###
##########################################################################


## required libraries for code ##
library(readxl);
library(data.table);
library(stringr);

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

process_phenology_data <- function(path, dirs){
	for (i in 1:length(paths)){
		phenology_excel_to_csv(paths[i])
	}
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
	print(Nuolja.Data)

	## Rename fields to match Nuolja Project MS Access Database ##
	colnames(Nuolja.Data) <- c("Synonym Current","Date","Subplot","Code")


	##Create text file of the newly formatted dataset ##
	print(dir)
	# TODO Compare to the original results
	# write.csv(Nuolja.Data, "Nuolja.Data.csv", row.names=FALSE)
}

