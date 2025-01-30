# TODO install all dependencies in the script
## Install geosphere package if not already install on client
list.of.packages <- c("geosphere", "dplyr", "data.table", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(geosphere);
library(dplyr);
source("R/helper.R");
source("R/repack.R");
source("R/generate.R");
source("R/validate.R");

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
		dirs <- dirs[datatypes == datatypes[as.numeric(answer)]]
		paths <- paths[datatypes == datatypes[as.numeric(answer)]]
		datatypes <- datatypes[datatypes == datatypes[as.numeric(answer)]]
		break;
	}
	print("Invalid input, please try again");
}

for (i in 1:length(datatypes)){
	createDir(paste("repack/", datatypes[i], sep=""))
	createDir(paste("out/", datatypes[i], sep=""))
}

# paths <- paths[grepl("Raw Data$|\\d{4}$", dirs)]
# dirs <- dirs[grepl("Raw Data$|\\d{4}$", dirs)]
paths <- paths[grepl("Raw Data$", dirs)]
dirs <- dirs[grepl("Raw Data$", dirs)]

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

for(i in 1:length(paths)){
	regex = "\\d{4}.csv$"
	if(!silent && promt){
		## promt for regex
		print("Use default filter (1) or custom filter (2)?")
		print("Default filter: ", regex)
		answer <- readLines(file("stdin"), 1)
		if(answer == "2"){
			print("Enter filter regex: ")
			regex <- readLines(file("stdin"), 1)
		}
	}
	path = getDataFilesPaths(paths[i], pattern=regex);
	if(!silent){
		print(paste("Directory :", dirs[i]));
		if(promt) {
			print("Do you wanna process this directory? (y/n)");
			answer <- readLines(file("stdin"),1)
			if(answer != "y") return(NULL);
			print(paste("Processing :", paths[i]));
		}
	}
	outputfile = gsub("/Raw Data", "",dirs[i]);
	data <- exportCSV(path, paste("repack/", outputfile, sep=""));
	if(validate){
		next;
	}
	if(is.null(data)) next;
	if(!silent){
		print(paste("Completed -", dirs[i]));
		print(paste("Location -", getwd()));
	}
	# drawPlots(data$result);
	write.csv(data$result, paste(data$filename, ".csv", sep=""), row.names=FALSE)


	data$result = data$result %>% arrange(date);
	#print(data$result[1:15,c(1,2,5,9,10)]);
	# cont_plot <- calculate(data$result, "plot", "contemporary")
	# print(cont_plot[1:10,]);
	# # cont_plot <- calculate(data$result, "subplot", "contemporary")
	# print(cont_plot[1:10,]);
	# TODO call old library to calculate the data

	subplot = subCalc(data$result[-1], dirs[i], transect_desc[,c(2,3)]);
	filename = gsub("repack/", "", data$filename)
	write(subplot$contemporary, paste(filename, "_contemporary", sep=""), "subplot");
	write(subplot$historical, paste(filename, "_historical", sep=""), "subplot");


	plotcoord = matrix(,nrow=0,ncol=2)
	for(i in 1:20){
		tmp = transect_desc[transect_desc[,1]==i,]
		res = c(i, min(tmp[,3]));
		if(i==20){
			res = rbind(res, c(i+1, max(tmp[,3])))
		}
		plotcoord = rbind(plotcoord, res)
	}
	plot = subCalc(data$result[-2], filename, plotcoord);

	write(plot$contemporary, paste(filename, "_contemporary", sep=""), "plot");
	write(plot$historical, paste(filename, "_historical", sep=""), "plot");

}

print("repack.r : DONE : CSV files built");


