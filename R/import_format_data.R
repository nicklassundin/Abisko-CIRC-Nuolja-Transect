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


read_excel_allsheets <- function(filename) {
	sheets <- readxl::excel_sheets(filename)
	x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, trim_ws = TRUE, col_names = FALSE, .name_repair = "minimal"))
	names(x) <- sheets
	x
}

import_nuolja_phenology <- function(input_file) {

	## Import all sheets from each Excel spreadsheet
	Nuolja.Data <- read_excel_allsheets(input_file)

	## Flatten sheets/cells into one column, then split comma-delimited rows
	Nuolja.Data <- data.table::rbindlist(
					     lapply(Nuolja.Data, function(x) data.table::as.data.table(x)),
					     fill = TRUE

	)

	Nuolja.Data <- data.table::data.table(
					      var = as.character(unlist(Nuolja.Data, use.names = FALSE))

	)

	Nuolja.Data <- Nuolja.Data[
				   !is.na(var) & trimws(var) != ""

				   ][
				   , data.table::tstrsplit(var, ",", fixed = TRUE)

				   ]

	## Delete all null records
	Nuolja.Data <- Nuolja.Data[rowSums(is.na(Nuolja.Data)) != ncol(Nuolja.Data), ]

	required_cols <- 8

	## Add missing columns
	while (ncol(Nuolja.Data) < required_cols) {
		Nuolja.Data[, paste0("V", ncol(Nuolja.Data) + 1) := NA_character_]

	}

	## Keep only first 8 columns
	Nuolja.Data <- Nuolja.Data[, 1:8]

	## Assign names
	colnames(Nuolja.Data) <- c(
				   "Date", "Subplot", "Species",
				   "Code.1", "Code.2", "Code.3", "Code.4", "Code.5"

	)

	## Convert Date
	Nuolja.Data$Date <- as.Date(Nuolja.Data$Date, format = "%d/%m/%Y")

	## Clean species
	Nuolja.Data$Species <- trimws(Nuolja.Data$Species, which = "right")

	## Extract species name
	Nuolja.Data$SpeciesName <- stringr::word(
						 Nuolja.Data$Species,
						 start = 1,
						 end = 2,
						 sep = stringr::fixed(" ")

	)

	## Reorder variables
	Nuolja.Data <- Nuolja.Data[, c(
				       "SpeciesName", "Date", "Subplot",
				       "Code.1", "Code.2", "Code.3", "Code.4", "Code.5"

				       )]

	colnames(Nuolja.Data) <- c(
				   "Species", "Date", "Subplot",
				   "Code.1", "Code.2", "Code.3", "Code.4", "Code.5"

	)

	## Sort
	Nuolja.Data <- Nuolja.Data[
				   with(Nuolja.Data, order(Date, Subplot, Species)),

				   ]

	## Restructure dataset
	NDa <- Nuolja.Data[, c("Species", "Date", "Subplot", "Code.1")]
	colnames(NDa) <- c("Species", "Date", "Subplot", "Code")

	NDb <- Nuolja.Data[, c("Species", "Date", "Subplot", "Code.2")]
	colnames(NDb) <- c("Species", "Date", "Subplot", "Code")

	NDc <- Nuolja.Data[, c("Species", "Date", "Subplot", "Code.3")]
	colnames(NDc) <- c("Species", "Date", "Subplot", "Code")

	NDd <- Nuolja.Data[, c("Species", "Date", "Subplot", "Code.4")]
	colnames(NDd) <- c("Species", "Date", "Subplot", "Code")

	NDe <- Nuolja.Data[, c("Species", "Date", "Subplot", "Code.5")]
	colnames(NDe) <- c("Species", "Date", "Subplot", "Code")

	## Merge all code columns
	Nuolja.Data <- data.table::rbindlist(
					     list(NDa, NDb, NDc, NDd, NDe),
					     use.names = TRUE,
					     fill = TRUE

	)

	## Remove records with no observations
	Nuolja.Data <- Nuolja.Data[
				   !(is.na(Code) | Code == ""),

				   ]

	## Remove spaces in Code column
	Nuolja.Data$Code <- gsub(" ", "", Nuolja.Data$Code)

	## Sort
	Nuolja.Data <- Nuolja.Data[
				   with(Nuolja.Data, order(Date, Subplot, Species)),

				   ]

	## Rename fields to match MS Access database
	colnames(Nuolja.Data) <- c(
				   "Synonym Current", "Date", "Subplot", "Code"

	)

	return(Nuolja.Data)

}

