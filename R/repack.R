list.of.packages <- c("geosphere");
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])];
if(length(new.packages)) install.packages(new.packages);
library(geosphere);


getPaths <- function(dir = "/data", pattern = "", full.names = TRUE) {
	paths <- list.dirs(paste(getwd(), dir, sep=""), full.names = TRUE)[-1]
	paths <- paths[grepl(pattern, paths)]
	return(paths)
}

getDirs <- function(dir = "/data", pattern = "", full.names = FALSE) {
	dirs <- list.dirs(paste(getwd(), dir, sep=""), full.names = full.names)[-1]
	dirs <- dirs[grepl(pattern, dirs)]
	return(dirs)
}

getDataFilesPaths <- function(dir, pattern = "*.csv", full.names = TRUE) {
	files <- list.files(dir, pattern = pattern, full.names = full.names)
	return(files)
}


#' filter to Historical perspective
historical = function(y){
      tmp = tolower(as.vector(y));
      if(tmp %in% "so" || tmp %in% "s"|| tmp %in% "os") tmp = c("s");
      if(tmp %in% "r") tmp = c("o");
      return(tmp);
}
#' filter out 
contemporary = function(z){
	tmp = tolower(as.vector(z));
	if(tmp %in% "r") tmp = "o";
	return(tmp);
}

#' filter out none numerical used for coordinates who use the E & N notation.
noneNum = function(y){
      return(lapply(y, function(x) as.double(gsub("[^0-9\\.]", "", y))));
}

createDateAndFix = function(string){
	if(length(string) > 7) {
		return(as.Date(string, "%Y%m%d"));
	}else if(length(string) == 7){
		return(as.Date(string, "%Y%m%d"));
	}
}

#' function that destill dates from format type NS-20190505-033 and NS20200805-033 and return the date in the format 2020-05-05
formatDate = function(x){
	result = x;
	l = nchar(strsplit(x, "-")[[1]][1]);
	# print(nchar(x));
	# print(x);
	if(l <= 4 && nchar(x) >= 10) {
		result = as.Date(substr(x, 2+l, 10+l), "%Y%m%d");
	}else if(l== 8){
		result = as.Date(paste("20", substr(x, 3, 8), sep=""), "%Y%m%d");
	}else {
		result = as.Date(substr(x, 3, 10), "%Y%m%d");
	}
	# print(result);
	return(result);
}

readFile = function(x){
	entries = read.delim(x, header=FALSE, sep=",")[,1:5]
	if(dim(entries)[2] < 5) return(NA);	
	# dates = lapply(as.character(entries[,1]), function(x) return(strsplit(x,"-")[[1]][2]));
	# dates = as.Date(delist(dates), "%Y%m%d");
	dates = delist(lapply(as.character(entries[,1]), formatDate));
	# print(dates)
	## if we want year adn DOY in this file 
	# years = format(dates, "%Y");
	# dates = format(dates, "%j");
	# dates = cbind(years, dates)
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
	
#' inserts new entry to target and return target
insert <- function(target, entry, sect){
	if(is.null(target)){
		target <- cbind(t(sect), entry);
	}else{
		target <- rbind(target, cbind(t(sect), entry));
	}
	return(target);
}

drawPlots <- function(df) {
	df$date = as.numeric(format(as.Date(df$date, origin="1970-01-01")), "%j");
			print(df[1:5])
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

dataframeBuilder <- function(data){
	# print(data)
	# initating result data.frame
	result <- data.frame();
	# print(result)
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
	# print(result[1:26,])
	return(result);
}


