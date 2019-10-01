## Script will create sub files for all Snow_Data_YYYY.csv in current directory
## name convention will be YYYY_[historical/contemporary]_[plot/subplot].csv



library(data.table)

## formating header and finding matching files in current directory to pattern "Snow_Data_*"
filenames <- list.files(getwd(), pattern = "*.csv", TRUE);
filenames <- subset(filenames, grepl("Snow_Data_*", filenames));
filenames <- gsub(".csv", "", filenames);
filenames <- gsub("[^0-9\\.]", "", filenames);
files <- list.files(getwd(), pattern = "*.csv", full.names = TRUE);
files <- subset(files, grepl("Snow_Data_*", files));
#############


data <- lapply(files, function(x) read.delim(x, header=TRUE, sep=","));

keyset  <- list(
		contemporary = c("s", "so", "o", "os"),
		historical = c("s", "o"))

match <- function(entries, key){
	entries = vapply(entries, paste, collapse = ", ", character(1L));
	# print(entries)
	# print(key)
	result = vapply(lapply(entries, function(x){
				       return(identical(as.vector(x), key));
}), paste, collapse = ', ', character(1L))
	# print(result)
	return(result)
}
# match(data.frame(data[[1]])$contemporary, "os")


genPercen <- function(entries, keys){
	result = c();
	name = colnames(entries);
	filter = function(x) return(NA);
	if(name %in% "historical"){
		filter = function(x) return(entries[(historical %in% x),])
	}else if(name %in% "contemporary"){
		filter = function(x) return(entries[(contemporary %in% x),])
	}
	# print(nrow(filter("o"))/nrow(entries))
	for(k in keys){
		result = c(result, nrow(filter(k))/nrow(entries));	
	}
	# print(result)
	return(result)
}
# genPercen(data.table(data[[1]])[plot==20], keyset$historical)
# genPercen(data.table(data[[1]])[plot==20], keyset$contemporary)
# data.table(data[[1]])[plot==20][,"historical"]
# genPercen(data.table(data[[1]])[plot==20][,"historical"], keyset$historical)
# genPercen(data.table(data[[1]])[plot==20][,"contemporary"], keyset$contemporary)


subCalc <- function(entries, type, plot){
	days = unique(entries$date);
	entries = data.table(entries);
	n = 0;		# number of plots 
	ne = 0; 	# how many plots should exist 
	keys = NA;
	getSubE = function(e,x,y) return(NA);
	if(type %in% "historical"){
		keys = keyset$historical;
	}else if(type %in% "contemporary"){
		keys = keyset$contemporary;
	}
	if(plot %in% "plot") {
		n = 20;
		ne = 4;
		if(type %in% "historical") getSubE = function(e,x,y) return(e[date==y ][ plot==x][,"historical"]);
		if(type %in% "contemporary") getSubE = function(e,x,y) return(e[date==y ][ plot==x][,"contemporary"]);
	}
	if(plot %in% "subplot"){
		n = 78;
		ne = 1;
		if(type %in% "historical") getSubE = function(e,x,y) return(e[date==y ][ subplot==x][,"historical"]);
		if(type %in% "contemporary") getSubE = function(e,x,y) return(e[date==y ][ subplot==x][,"contemporary"]);
	}
	result <- matrix(,nrow=0,ncol=length(keys)+2);
	for(plot in 1:n){
		for(day in days){
			sub_e = getSubE(entries,plot,day);
			# enter the 'open ground' points between	
			while(nrow(sub_e)<ne){
				sub_e = rbind(sub_e, list("o"));
			}
			sub_e = genPercen(sub_e, keys);
			result = rbind(result, c(day, plot, sub_e));
		}
	}
	result = data.table(result);
	colnames(result) <- c("date", plot, keys);
	# print(colnames(result))
	return(result)
}

buildCSV <- function(dataset, filename){
	# print(filename)
	write.csv(subCalc(dataset, "contemporary", "plot"), paste(filename, "_contemporary_plot.csv", sep=""), row.names=FALSE);
	write.csv(subCalc(dataset, "contemporary", "subplot"), paste(filename, "_contemporary_subplot.csv", sep=""), row.names=FALSE)
	write.csv(subCalc(dataset, "historical", "plot"), paste(filename, "_historical_plot.csv", sep=""), row.names=FALSE);
	write.csv(subCalc(dataset, "historical", "subplot"), paste(filename, "_historical_subplot.csv", sep=""), row.names=FALSE)
}

for(i in 1:length(files)){
	buildCSV(data[[i]], filenames[i]);
}
print("Done");
