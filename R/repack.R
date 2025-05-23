#' @title Get Paths
#' @description Retrieve the paths of directories within a given directory, optionally matching a pattern.
#' @param dir A character string representing the directory to search within. Default is "/data".
#' @param pattern A character string representing the pattern to match. Default is an empty string.
#' @param full.names A logical value indicating whether to return full names of the directories. Default is TRUE.
#' @return A character vector of directory paths matching the pattern.
#' @export
getPaths <- function(dir = "/data", pattern = "", full.names = TRUE) {
	paths <- list.dirs(paste(getwd(), dir, sep=""), full.names = TRUE)[-1]
	paths <- paths[grepl(pattern, paths)]
	return(paths)
}

#' @title Get Directories
#' @description Retrieve the names of directories within a given directory, optionally matching a pattern.
#' @param dir A character string representing the directory to search within. Default is "/data".
#' @param pattern A character string representing the pattern to match. Default is an empty string.
#' @param full.names A logical value indicating whether to return full names of the directories. Default is FALSE.
#' @return A character vector of directory names matching the pattern.
#' @export
getDirs <- function(dir = "/data", pattern = "", full.names = FALSE) {
	dirs <- list.dirs(paste(getwd(), dir, sep=""), full.names = full.names)[-1]
	dirs <- dirs[grepl(pattern, dirs)]
	return(dirs)
}

#' @title Get Data Files Paths
#' @description Retrieve the paths of data files within a given directory, optionally matching a pattern.
#' @param dir A character string representing the directory to search within.
#' @param pattern A character string representing the pattern to match. Default is "*.csv".
#' @param full.names A logical value indicating whether to return full names of the files. Default is TRUE.
#' @return A character vector of file paths matching the pattern.
#' @export
getDataFilesPaths <- function(dir, pattern = "*.csv", full.names = TRUE) {
	files <- list.files(dir, pattern = pattern, full.names = full.names)
	return(files)
}

#' @title Historical Filter
#' @description Filter input to identify historical perspective.
#' @param y A vector of characters to filter.
#' @return A character vector with historical perspective.
#' @export
historical = function(y){
	tmp = tolower(as.vector(y));
	if(tmp %in% "so" || tmp %in% "s"|| tmp %in% "os") tmp = c("s");
	if(tmp %in% "r") tmp = c("o");
	return(tmp);

}

#' @title Contemporary Filter
#' @description Filter input to identify contemporary perspective.
#' @param z A vector of characters to filter.
#' @return A character vector with contemporary perspective.
#' @export
contemporary = function(z){
	tmp = tolower(as.vector(z));
	if(tmp %in% "r") tmp = "o";
	return(tmp);

}

#' @title Filter Non-numerical Characters
#' @description Filter out non-numerical characters from input.
#' @param y A vector of characters to filter.
#' @return A vector of double values after removing non-numerical characters.
#' @export
noneNum = function(y){
	return(lapply(y, function(x) as.double(gsub("[^0-9\\.]", "", y))));

}

#' @title Create Date and Fix
#' @description Convert a string to a Date object, fixing format if necessary.
#' @param string A character string representing a date.
#' @return A Date object.
#' @export
createDateAndFix = function(string){
	if(length(string) > 7) {
		return(as.Date(string, "%Y%m%d"));

	}else if(length(string) == 7){
		return(as.Date(string, "%Y%m%d"));

	}

}

#' @title Format Date
#' @description Extract and format dates from a specific string format.
#' @param x A character string representing a date.
#' @return A Date object.
#' @export
formatDate = function(x){
	result = x;
	l = nchar(strsplit(x, "-")[[1]][1]);
	if(l <= 4 && nchar(x) >= 10) {
		result = as.Date(substr(x, 2+l, 10+l), "%Y%m%d");

	}else if(l== 8){
		result = as.Date(paste("20", substr(x, 3, 8), sep=""), "%Y%m%d");

	}else {
		result = as.Date(substr(x, 3, 10), "%Y%m%d");

	}
	return(result);

}

#' @title Extract Date from Filename
#' @description Extract the date from a filename in YYYYMMDD format.
#' @param filename A character string representing the filename.
#' @return A character string representing the extracted date, or NULL if no date is found.
#' @export
extract_date <- function(filename) {
	# Ensure the input is just the filename
	filename <- basename(filename)  # Extracts the filename from a full path

	# Define patterns to match the date in YYYYMMDD or YYMMDD formats followed by .csv
	pattern_1 <- "([0-9]{8})\\.csv$"  # Matches YYYYMMDD
	pattern_2 <- "([0-9]{6})\\.csv$"  # Matches YYMMDD

	# Try to match the first pattern
	match <- regmatches(filename, regexec(pattern_1, filename))

	# If the first pattern does not match, try the second pattern
	if (length(match[[1]]) == 0) {
		match <- regmatches(filename, regexec(pattern_2, filename))

	}

	# If neither pattern matches, return NULL
	if (length(match[[1]]) == 0) {
		return(NULL)

	}

	# Return the matched date
	return(match[[1]][2])

}

#' @title Read File
#' @description Read and process a data file.
#' @param x A character string representing the file path.
#' @return A processed data frame.
#' @export
readFile = function(x, valid){
	# check if any row are valid
	if(all(!valid)) return(NA);
	# extract date from file name x
	date = as.character(as.Date(extract_date(x), "%Y%m%d"));
	# check for header
	entries = read.delim(x, header=FALSE, sep=",")[,1:5]
	# filter rows based on valid
	entries = entries[valid,];
	if(dim(entries)[2] < 5) return(NA);	

	# create a vector of length if entries column filled with date
	dates = rep(date, nrow(entries));
	###########
	entries[,2] = delist(lapply(entries[,2], noneNum));
	entries[,3] = delist(lapply(entries[,3], noneNum));
	entries[,5] = delist(lapply(entries[,5], contemporary));
	hist = data.matrix(lapply(entries[,5], historical));
	# unlisting to be able to bind it to 'entries'
	hist <- delist(hist)

	entries = cbind(entries, hist);
	entries = cbind(entries[,1], cbind(dates, entries[,-1]));
	# return full structure
	return(entries);

} 

#' @title Insert Entry
#' @description Insert a new entry into a target and return the updated target.
#' @param target A data frame representing the target.
#' @param entry A data frame representing the entry to insert.
#' @param sect A vector representing the section.
#' @return The updated target data frame.
#' @export
insert <- function(target, entry, sect){
	if(is.null(target)){
		target <- cbind(t(sect), entry);

	}else{
		target <- rbind(target, cbind(t(sect), entry));

	}
	return(target);

}

#' @title Draw Plots
#' @description Generate and save plots based on the provided data frame.
#' @param df A data frame containing the data to plot.
#' @return None
#' @export
drawPlots <- function(df) {
	df$date = as.numeric(format(as.Date(df$date, origin="1970-01-01")), "%j");
	for(d in unique(df$date)){
		temp = df[df$date == d,];
		if(nrow(temp)>1){
			plot(temp[c(3,5)],type="b", pch=as.character(temp$historical),
			     ylim=rev(range(df[,5])),
			     xlim=range(df[,3]))

		}
		par(new=TRUE)

	}
	par(new=FALSE)
	pdf("plots/Snow_Data_Plot.pdf",width=20,height=5);

}

#' @title Build Data Frame
#' @description Build a data frame by accumulating rows from provided data.
#' @param data A list of data frames to accumulate.
#' @return A data frame containing the accumulated data.
#' @export
dataframeBuilder <- function(data){
	# initating result data.frame
	result <- data.frame();
	# Accumulative build result row by row, with insert(,,);
	for(i in 1:length(data)){
		if(length(data[[i]][!is.na(data[[i]])]) > 0){
			for(j in 1:nrow(data[[i]])){
				clos <- closest(data[[i]][j,3:5], transect_desc);
				result <- insert(result, data[[i]][j,], clos);

			}

		} 

	}
	## naming the columns for the return file
	colnames(result) <- colnames; 
	rownames(result) <- 1:nrow(result);
	# mutate in Poles into result from plots_and_subplots table
	result <- merge(result, plots_and_subplots, by.x = c("plot", "subplot"), by.y = c("Plot", "Subplot"), all.x = TRUE)
	# reorder the columns so Last column are first
	# result <- result[,c(ncol(result), 1:(ncol(result)-1))];
	return(result);

}
