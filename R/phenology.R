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

datasheet_info <- read.xlsx(datasheet_info, sheet = 1, colNames = TRUE)



survey_data_sheet_get <- function(species_list, poles, i){

	poles_species <- species_list %>%
		filter(`Poles` == poles[i] | `Poles` == poles[i+1]) %>%
		distinct(`Synonym Current`, Poles)
	poles_species <- poles_species %>%
		group_by(`Synonym Current`) %>%
		mutate(PoleNum = case_when(`Poles` == poles[i] ~ "1",
					   `Poles` == poles[i + 1] ~ "2")) %>%
		left_join(datasheet_info, by = c("Synonym Current" = "Species")) %>%
		group_by(`Synonym Current`) %>%
		summarise(
			  PoleNums = sort(unique(PoleNum)),
			  num_poles = n_distinct(PoleNum),
			  W = any(W == "Y", na.rm = TRUE),
			  WG = any(WG == "Y", na.rm = TRUE),
			  .groups = "drop"
			  ) %>%
		mutate(tag = case_when(num_poles == 2 ~ "",  # appears in both — omit tag
				       W & WG ~ paste0("(", PoleNums[1], ", W, WG)"),
				       W ~ paste0("(", PoleNums[1], ", W)"),
				       WG ~ paste0("(", PoleNums[1], ", WG)"),
				       TRUE ~ ""
				       ),`Synonym Current` = if_else(tag != "",
				       paste0(`Synonym Current`, " ", tag),
				       `Synonym Current`));
		poles_species <- poles_species %>%
			distinct(`Synonym Current`);
	return(poles_species)
}

#' @title survey_tables
#' @description This function creates survey tables for the given data frame
#' @param df The data frame to create survey tables for
#' @return A data frame containing the survey tables
#' @export


survey_tables <- function(df){

	species_counts <- df %>%
		distinct(`Synonym Current`, Poles, Year) %>%
		count(`Synonym Current`, Poles);
	species_list <- df %>% 
		group_by(`Synonym Current`, Poles, Year) %>% 
		summarise(n = n(), .groups = "drop") %>%
		select(-n)
	species_list <- species_list %>%
		left_join(species_counts, by = c("Synonym Current", "Poles")) %>%
		filter(n > 1)


	# Observation Error
	species_errors <- read.xlsx(species_errors, sheet = 1, colNames = TRUE)
	species_corrected_list <- species_errors$`Observed.species` %>% unique()
	# remove columns
	species_errors <- species_errors[,c(1,2,3,4,5,7,8,9)]
	# print(species_errors[1:10,])
	# filter based on species errors 'Species Error (Y/N)' is Y


	species_list <- species_list %>%
		left_join(species_errors, 
			  by = c("Synonym Current" = "Observed.species", "Year" = "Year", "Poles" = "Subplot")) 
	# by = c("Synonym Current" = "Observed.species", "Year" = "Year", "Poles" = "Subplot")) 

	# print(species_list[,2])
	mask = !is.na(species_list$`Corrected.name`) & species_list$`Species.Error.(Y/N)` == "Y"; 
	species_list[mask,]$`Synonym Current` = species_list[mask,]$`Corrected.name`
	species_list <- species_list %>%
		mutate(`Field Filter` = 
		       (!is.na(`Corrected.name`) & (`Species.Error.(Y/N)` == "Y")) | (`Species.Error.(Y/N)` == "N") &
		       (`single.date.observation.(Y/N)` == "N") &
		       (`High.confidence.of.correct.identification.on.species.level.(Y/N)` == "Y"))

	species_list <- species_list %>%
		filter(`Field Filter` == TRUE)

	species_list <- species_list[order(species_list$Poles),]
	# list of poles
	poles <- species_list %>%
		distinct(Poles, .keep_all = TRUE) %>%
		select(Poles);
	# poles to array
	poles <- as.character(poles$Poles)

	# print(poles[1:10])
	# Boolean table for Poles
	species_list <- species_list[order(species_list$`Synonym Current`),]
	# Exclude if not in corrected species list
	species_list <- species_list %>%
		filter(`Synonym Current` %in% species_corrected_list)

	# select(-W) %>% select(-WG)
	# print(species_list[,c(1,12:20)])

	# print(colnames(species_list)[11:20])
	# return(TRUE)
	wb <- createWorkbook()
	dir.create("out/Planet Phenology Survey", showWarnings = FALSE, recursive = TRUE)

	top_header <- matrix(c("Date:", "Surveyors:", ""), nrow = 1)
	phen_sub_head <- c("Confirmed ID", "Leaf-out", "Flowering", "Fruiting", "Seed Dispersal", "Senescence", "Leaf Fall")
	phen_header <- matrix(c("Phenology Phases", phen_sub_head, phen_sub_head), nrow = 1)
	# iterate over the poles by pair neigboors
	for (i in seq(1, length(poles), 2)){
		sheet = paste0(substr(poles[i],1,2), "-", substr(poles[i+1],7,8))
		addWorksheet(wb, sheet);
		# Create a centering style
		centerStyle <- createStyle(halign = "center", valign = "center")
		# Apply centering to the whole used range
		addStyle(wb, sheet = sheet, style = centerStyle,
			 rows = 2:3, cols = 2:15, gridExpand = TRUE)

		setColWidths(wb, sheet = sheet, cols = 1, widths = 35)
		writeData(wb, sheet, x = top_header, startCol = 1, startRow = 1, colNames = FALSE)
		mergeCells(wb, sheet, cols = 2:15, rows = 1)
		poles_header <- matrix(c("Subplot", poles[i],"","","","","","", poles[i+1]), nrow = 1)
		writeData(wb, sheet, x = poles_header, startCol = 1, startRow = 2, colNames = FALSE)
		mergeCells(wb, sheet, cols = 2:8, rows = 2)
		mergeCells(wb, sheet, cols = 9:15, rows = 2)
		writeData(wb, sheet, x = phen_header, startCol = 1, startRow = 3, colNames = FALSE)
		# Create vertical text style (textRotation = 90 for vertical)
		verticalStyle <- createStyle(textRotation = 90, halign="center", textDecoration = "bold", 
					     valign = "center", wrapText = TRUE, fontSize = 10)
		# Apply style to header row (row 2)
		setRowHeights(wb, sheet = sheet, rows = 3, heights = 75)
		addStyle(wb, sheet = sheet, style = verticalStyle, 
			 rows = 3, cols = 2:15, gridExpand = TRUE)
		# Bold for top rows
		boldStyle <- createStyle(textDecoration = "bold", valign = "center")
		addStyle(wb, sheet = sheet, style = boldStyle,
			 rows = 2, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		addStyle(wb, sheet = sheet, style = boldStyle,
			 rows = 3, cols = 1, gridExpand = TRUE, stack = TRUE)
		# Bold for top header
		addStyle(wb, sheet = sheet, style = boldStyle,
			 rows = 1, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		setRowHeights(wb, sheet = sheet, rows = 1, heights = 25)
		poles_species <- survey_data_sheet_get(species_list, poles, i)
		writeData(wb, sheet, x = poles_species$`Synonym Current`, startCol = 1, startRow = 4, colNames = FALSE)

		# Create fill style
		fillStyle <- createStyle(
					 fgFill = "#D9D9D9",
		)

		# Get total number of rows (including header)
		total_rows <- nrow(poles_species) + 4  # +1 for header

		# Apply style in blocks: color 3 rows, skip 3
		for (start_row in seq(4, total_rows, by = 6)) {
			rows_to_color <- start_row:min(start_row + 2, total_rows)
			addStyle(wb, sheet = sheet, style = fillStyle,
				 rows = rows_to_color, cols = 1:15, gridExpand = TRUE)

		}
		gridStyle <- createStyle(border = "TopBottomLeftRight", borderStyle = "thin")
		addStyle(wb, sheet = sheet, style = gridStyle,
			 rows = 2:100, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		gridStyleThick <- createStyle(border = "TopBottomLeftRight", borderStyle = "medium")
		addStyle(wb, sheet = sheet, style = gridStyleThick,
			 rows = 1:3, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		addStyle(wb, sheet = sheet, style = gridStyleThick,
			 rows = 1:100, cols = 1, gridExpand = TRUE, stack = TRUE)

	}
	saveWorkbook(wb, "out/Planet Phenology Survey/test.xlsx", overwrite = TRUE)
}

