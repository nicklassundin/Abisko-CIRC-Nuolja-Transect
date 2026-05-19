##########################################################################
### Import Nuolja phenology data and then do some basic formating ###
##########################################################################
### Last modified by Emilien Depraz on 12th August 2025                    ###
##########################################################################

## required libraries for code ##
library(readxl)    
library(data.table)
library(plyr)
library(stringr)


import_nuolja_phenology <- function(input_file) {
	## create function to read in all sheets from an Excel spreadsheet ##
	read_excel_allsheets <- function(filename) {
		sheets <- readxl::excel_sheets(filename)
		x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, range = ("C140:ED272"),trim_ws = TRUE, col_names = FALSE, .name_repair = "minimal"))
		names(x) <- sheets
		x

	}

	print(input_file)

	## Import all sheets from each Excel spreadsheet ##
	Nuolja.Data <- read_excel_allsheets(input_file)
	
	## split data in the single column into multiple columns by the comma delimiter ##
	Nuolja.Data <- setDT(Nuolja.Data)[, list(var = unlist(.SD))][, tstrsplit(var, ",")]
	
	## delete all null records, e.g. all data in a record is NA ##
	Nuolja.Data <- Nuolja.Data[rowSums(is.na(Nuolja.Data))!=ncol(Nuolja.Data), ]

	required_cols <- 8

	# add missing columns
	while (ncol(Nuolja.Data) < required_cols) {
		Nuolja.Data[, paste0("V", ncol(Nuolja.Data) + 1) := NA_character_]
	}

	## assign names to columns ##
	## ADD names if new columns needed
	print(Nuolja.Data)
	colnames(Nuolja.Data) <- c("Date","Subplot","Species","Code.1","Code.2","Code.3", "Code.4", "Code.5")
	

	## convert character to date for Date column ##
	Nuolja.Data$Date <- as.Date(Nuolja.Data$Date, format="%d/%m/%Y")

	## remove whitespace at end of species names ##
	Nuolja.Data$Species <- trimws(Nuolja.Data$Species, which = c("right"))

	## extract only the species name, not the variety or subspecies ##
	Nuolja.Data$SpeciesName <- word(Nuolja.Data$Species, start=1, end =2, sep=fixed(" "))

	## reorder and rename variables ##
	## ADD COLUMNS HERE IF ERROR APPEARS: ROWS and Code.3 etc.
	Nuolja.Data <- Nuolja.Data[,c(9,1,2,4,5:8)]
	colnames(Nuolja.Data) <- c("Species","Date","Subplot","Code.1","Code.2", "Code.3", "Code.4", "Code.5")

	## sort data set by date, Subplot and species ##
	Nuolja.Data <- Nuolja.Data[with(Nuolja.Data, order(Date, Subplot, Species)), ]
	

	## restructure dataset splitting files and renaming the 'code' field ##
	## REMOVE # HERE IF ERROR APPEARS
	NDa <- Nuolja.Data[,1:4]
	colnames(NDa) <- c("Species","Date","Subplot","Code")
	NDb <- Nuolja.Data[,c(1:3,5)]
	colnames(NDb) <- c("Species","Date","Subplot","Code")
	NDc <- Nuolja.Data[,c(1:3,6)]
	colnames(NDc) <- c("Species","Date","Subplot","Code")
	NDd <- Nuolja.Data[,c(1:3,7)]
	colnames(NDd) <- c("Species","Date","Subplot","Code")
	NDe <- Nuolja.Data[,c(1:3,8)]
	colnames(NDe) <- c("Species","Date","Subplot","Code")
	#NDf <- Nuolja.Data.2022[,c(1:3,9)]
	#colnames(ND.2022f) <- c("Species","Date","Subplot","Code")
	#NDg <- Nuolja.Data.2022[,c(1:3,10)]
	#colnames(ND.2022g) <- c("Species","Date","Subplot","Code")

	## merge to create new dataset ##
	##ADD ND...
	Nuolja.Data <- rbind(NDa, NDb, NDc)

	## remove records with no observations
	Nuolja.Data <- Nuolja.Data[!(is.na(Nuolja.Data$Code) | Nuolja.Data$Code == ""),]

	## Remove spaces in Code column
	Nuolja.Data$Code <- gsub(" ", "", Nuolja.Data$Code)

	## sort data set by date, Subplot and species ##
	Nuolja.Data <- Nuolja.Data[with(Nuolja.Data, order(Date, Subplot, Species)), ]

	## Rename fields to match Nuolja Project MS Access Database ##
	colnames(Nuolja.Data) <- c("Synonym Current","Date","Subplot","Code")

	##Create text file of the newly formatted dataset ##
	# write.csv(Nuolja.Data, output_file, row.names=FALSE)
	
	return(Nuolja.Data)



}
