# TODO install all dependencies in the script
## Install geosphere package if not already install on client
list.of.packages <- c("geosphere", "dplyr", "data.table", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(geosphere);
library(dplyr);
library(data.table);
source("R/helper.R");
source("R/snow.R");
source("R/repack.R");
source("R/phenology.R");
# source("R/validation/phenology.R");
source("R/validation/snow.R");


createDir("data")

## Build paths for the directories
paths <- getPaths() 
dirs <- getDirs()

transect_desc = loadTransectDescription();

print("Select operations to perform");
print("0) Exit");
print("1) Build CSV files");
print("2) Build CSV with debug");
print("3) Validate CSV files");
print("4) Debug");
# loop until vallid input is given
while(TRUE){
	answer <- readLines(file("stdin"), 1);
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
validate = FALSE;
silent = TRUE;
promt = FALSE;
if(answer == "2"){
	print("Build CSV files with debug")
	silent = FALSE;
}else if(answer == "3"){
	print("Validation mode");
	validate = TRUE;
}else if(answer == "4"){
	print("Debug mode");
	silent = FALSE;
	promt = TRUE;
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
	print(paste(length(datatypes)+1,") All data", sep=""));
	answer <- readLines(file("stdin"), 1);
	answer <- gsub("\\)", "", answer);
	answer <- gsub("\\D", "", answer);
	if(answer == "0"){
		return(NULL);
	}
	if(as.numeric(answer) <= length(datatypes)+1){
		dirs <- dirs[grepl(datatypes[as.numeric(answer)], dirs)]
		paths <- paths[grepl(datatypes[as.numeric(answer)], paths)]
		datatypes <- datatypes[datatypes == datatypes[as.numeric(answer)]]
		break;
	}
	print("Invalid input, please try again");
}
for (i in 1:length(datatypes)){
	createDir(paste("repack/", datatypes[i], sep=""))
	createDir(paste("out/", datatypes[i], sep=""))
}

paths_snow <- paths[grepl("Raw Data$|Raw Data \\d{4}$", dirs)]
dirs_snow <- dirs[grepl("Raw Data$|Raw Data \\d{4}$", dirs)]
paths_phenology <- paths[grepl("Phenology Data$", dirs)]
dirs_phenology <- dirs[grepl("Phenology Data$", dirs)]

if(promt){
	print("Use default filter (1) or custom filter (2)?")
	answer <- readLines(file("stdin"), 1)
	if(answer == "2"){
		print("Enter filter regex: ")
		filter <- readLines(file("stdin"), 1)
		paths <- paths[grepl(filter, dirs)]
		dirs <- dirs[grepl(filter, dirs)]
	}
}


for (i in 1:length(datatypes)){
	if(datatypes[i] == "Plant Phenology Data"){
		process_phenology_data(paths, dirs)
	}else if(datatypes[i] == "Nuolja Snow Data"){
		dataframe <- process_snow_data(paths, dirs)
	}
}
