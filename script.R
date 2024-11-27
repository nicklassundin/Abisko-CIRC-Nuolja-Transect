## Install geosphere package if not already install on client
list.of.packages <- c("geosphere", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(geosphere);
library(dplyr);
source("R/helper.R");
source("R/repack.R");
source("R/generate.R");
source("R/validate.R");

# Creating missing directories for repack and outpu
createDir <- function(subdir){
	if(!file.exists(subdir)){
		# create a new sub directory inside
		# the main path
		dir.create(file.path(getwd(), subdir))
	}
}
createDir("repack")
createDir("out")
createDir("data")


## Build paths for the directories
paths <- getPaths() 
dirs <- getDirs()

transect_desc = loadTransectDescription();

print("Select operations to perform");
print("1) Build CSV files");
print("2) Build CSV with debug");
print("3) Validate CSV files");
print("4) Debug");
answer <- readLines(file("stdin"), 1);
validate = FALSE;
silent = TRUE;
promt = FALSE;
if(answer == "2"){
	silent = FALSE;
}else if(answer == "3"){
	validate = TRUE;
}else if(answer == "4"){
	silent = FALSE;
	promt = TRUE;
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

## this writes to csv file based on name called at the end of the document
exportCSV <- function(filenames, filename){
	if(length(filenames)==0) return(NULL)
	# Parsing the data from the file

	# entries = read.delim(filenames[1], header=FALSE, sep=",")[,1:5]
	valid = lapply(filenames, validateFile);
	# Read data only for valid files
	data <- mapply(readFile, filenames, valid, SIMPLIFY = FALSE)
	# data = lapply(filenames, readFile);
	# Accumulative build result row by row, with insert(,,);
	result <- dataframeBuilder(data);
	return(list(result=result, filename=filename));	
}


for(i in 2:length(paths)){
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
	outputfile = gsub(" ", "_", outputfile);
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


