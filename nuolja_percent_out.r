
files <- list.files(getwd(), pattern = "*.csv", full.names = TRUE);
files <- subset(files, grepl("Snow_Data_*", files));
# files;

getDate <- function(x) return(strsplit(x, "-")[[1]][2])
# getDate("NS-19210312-001")

data <- lapply(files, function(x) read.delim(x, header=TRUE, sep=","));


contemp <- c("s", "so", "o", "os");
histori <<- c("s", "o");

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
	for(k in keys){
		result = c(result, nrow(entries[entries$contemporary %in% k,])/nrow(entries));	
	}
	return(result)
}
genPercen(data.frame(data[[1]]), contemp)
