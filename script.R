

## Install geosphere package if not already install on client
list.of.packages <- c("geosphere", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(geosphere);
library(dplyr);
source("R/helper.R");
source("R/repack.R");
source("R/generate.R")



## Build paths for the directories
paths <- getPaths() 
dirs <- getDirs()

transect_desc = loadTransectDescription();

## this writes to csv file based on name called at the end of the document
exportCSV <- function(filenames, filename){
	# print(paste("Directory :", filename));
	# print(paste("Processing :", filenames));
	if(length(filenames)==0) return(NULL)
	# Parsing the data from the file
	data = lapply(filenames, readFile);

	# Accumulative build result row by row, with insert(,,);
	result <- dataframeBuilder(data);
	return(list(result=result, filename=filename));	
}


for(i in 2:length(paths)){
	path = getDataFilesPaths(paths[i])
	data <- exportCSV(path, paste("repack/", gsub(" ", "_",dirs[i]), sep=""));
	print(paste("Completed -", dirs[i]));
	print(paste("Location -", getwd()));
	
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
	# print(subplot$historical[1:10,]);
	# print(subplot$contemporary[1:10,]);

	
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


	print(plot)

	# write(contemporary, name, sep=""), type);
	# write(historical, name, sep=""), type);
}

print("repack.r : DONE : CSV files built");


