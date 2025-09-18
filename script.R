# TODO install all dependencies in the script
## Install geosphere package if not already install on client
list.of.packages <- c("geosphere", "dplyr", "data.table", "lubridate", "stringr", "tidyr", "openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org/")
library(geosphere);
library(dplyr);
library(data.table);
library(lubridate)
source("R/helper.R");
source("R/snow.R");
source("R/repack.R");
source("R/phenology.R");
source("R/phenology_survey.R");
# source("R/validation/phenology.R");
source("R/validation.R");

# Retrieve command-line arguments (excluding default ones)
args <- commandArgs(trailingOnly = TRUE)

# Check for the presence of '-debug'
silent <- !("-debug" %in% args)
promt <- ("-debug" %in% args)


createDir("data")

## Build paths for the directories
paths <- getPaths() 
dirs <- getDirs()

transect_desc = loadTransectDescription();
plots_and_subplots = read.csv("descriptions/plots_and_subplots.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")

print("Select operations to perform");
print("0) Exit");
print("1) Build CSV files");
print("2) Produce Survey Excel files");
# loop until vallid input is given
inout <- file("stdin")

while(TRUE){
	answer <- readLines(inout, 1);
	# replace ) with empty string
	answer <- gsub("\\)", "", answer);
	# replace none numeric with empty string
	answer <- gsub("\\D", "", answer);
	if(answer == "0"){
		return(NULL);
	}
	if(answer == "1" || answer == "2" || answer == "3" || answer == "4"){
		break;
	}
	print("Invalid input, please try again");
}

survey = FALSE;
if(answer == "2"){
	survey = TRUE;
}else{
	print("Build CSV files");
}

# split dirs between / and take index 0
datatypes <- sapply(strsplit(dirs, "/"), "[", 1)
datatypes <- unique(datatypes)

# Select which data to process
while(TRUE){
	print("Select data to process");
	print("0) Exit");
	for(i in 1:length(datatypes)){
		print(paste(i,") ", datatypes[i], sep=""));
	}
	# TODO FIXME OR REMOVE
	# print(paste(length(datatypes)+1,") All data", sep=""));
	answer <- readLines(inout, 1);
	answer <- gsub("\\)", "", answer);
	answer <- gsub("\\D", "", answer);
	if(answer == "0"){
		return(NULL);
	}
	if(as.numeric(answer) < length(datatypes)+1){
		dirs <- dirs[grepl(datatypes[as.numeric(answer)], dirs)]
		paths <- paths[grepl(datatypes[as.numeric(answer)], paths)]
		datatypes <- datatypes[datatypes == datatypes[as.numeric(answer)]]
		break;
	}else if(as.numeric(answer) == length(datatypes)+1){
		# TODO FIXME
		# break;
	}
	print("Invalid input, please try again");
}
for (i in 1:length(datatypes)){
	createDir(paste("repack/", datatypes[i], sep=""))
	createDir(paste("out/", datatypes[i], sep=""))
}

paths_snow <- paths[grepl("Raw Data$|Raw Data \\d{4}$", dirs)]
dirs_snow <- dirs[grepl("Raw Data$|Raw Data \\d{4}$", dirs)]

dir_phenology <- paste("data", dirs[1], sep="/")


for (i in 1:length(datatypes)){
	print(datatypes)
	# if datatypes[i] contain Phenology
	if(grepl("Phenology", datatypes[i], fixed = TRUE)){
		print("Processing Phenology Data")
		# reorder dir_phenology
		# put first poistion last
		output <- read_phenology_data(dir_phenology, all=survey)
		df <- output$data

		# filter out none may date for all years
		df_may <- output$data %>%
			  filter(month(Date) == 5)
		if(survey){
			df_full <- build_species_list(df)
			build_data_sheets(df_full$species_list, df_full$poles, file_name = "out/Plant Phenology Survey/Nuolja Transect Phenology Datasheets.xlsx")

			# may call
			df_spring <- build_species_list(df_may)
			build_spring_data_sheets(df_spring$species_list, df_spring$poles, file_name = "out/Plant Phenology Survey/Nuolja Transect Phenology Datasheets SPRING.xlsx")
			# Data entry Segments Sheet
			build_data_entry_segments(df_full$species_list, df_full$poles, file_name = "out/Plant Phenology Survey/Nuolja Transect Phenology Data Entry Segments 01 to 79.xlsx")
		}else{
			process_phenology_data(output, dir_phenology)
		}
	}else if(datatypes[i] == "Nuolja Snow Data"){
		print("Processing Snow Data")
		dataframe <- process_snow_data(paths_snow, dirs_snow)
	}
}

close(inout)
