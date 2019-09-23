## require nuolja_repack.r plots_and_subplots.csv and transect_description.csv

## geosphere INSTALL
list.of.packages <- c("geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(geosphere);




plotdiv <- read.csv(file="plots_and_subplots.csv");
transect_desc <- read.csv(file="transect_description.csv")[,1:6][];
transect_desc <- data.matrix(transect_desc);

paths <- list.dirs(getwd(),full.names = TRUE)[-1]
dirs <- list.dirs(getwd(), full.names = FALSE)[-1];

## this writes to csv file based on name
exportCSV <- function(filenames, filename){
	if(length(filenames)==0) return(NULL)
	data = lapply(filenames, function(x){
			       historical = function(y){
				       if(y %in% "so" || y %in% "s") return(c("s"));
				       if(y %in% "os" || y %in% "o") return(c("o"));
			       }
			       concurrent = function(z){
				       tmp = tolower(as.vector(z));
				       if(tmp %in% "r") tmp = "o";
				       return(tmp);
			       }
			       filter = function(y){
				       if(is.numeric(y)) return(y);
				       return(as.double(gsub("[^0-9\\.]", "", y)));
			       }
			       entries = read.delim(x, header=FALSE, sep=",")
			       if(dim(entries)[2] < 5) return(NA);
			       entries[,2] = lapply(entries[,2], filter);
			       entries[,3] = lapply(entries[,3], filter);
			       entries[,5] = vapply(entries[,5], paste, collapse = ", ", character(1L));
			       entries[,5] = (vapply(lapply(entries[,5], concurrent), paste, collapse = ', ', character(1L)))
			       hist = data.matrix(lapply(entries[,5], historical));
			       hist <- vapply(hist, paste, collapse = ", ", character(1L))
			       entries = cbind(entries[,1:5], hist);
			       return(entries);
} 
	);


	# Brute forces point form from file fineds closest points
	# assumes that transect is never turning >45 degrees
	closest <- function(e, chart, i=1){
		# print(c(e, i))
		sect <- function(n){
			return(transect_desc[n,3]);
		}
		d1 = distm(e, chart[i,], fun=distHaversine);
		d2 = distm(e, chart[i+1,], fun=distHaversine);
		d3 = distm(e, chart[i+2,], fun=distHaversine);
		# TODO new
		if(d1 < d2) return(c(sect(i), i));
		if(d2 < d3) return(c(sect(i+1), i+1));
		if(nrow(chart) >= i+2+2){
			return(closest(e, chart, i+2));
		}else{
			if(nrow(chart)>= i+2+1){
				return(closest(e, chart, i+1));
			}else{
				return(c(sect(i+2), i+2));
			}
		}
	};

	result <- data.frame();

	insert <- function(target, entry, sect){
		# print(entry)
		# print(length(entry))
		if(is.null(target)){
			target <- cbind(t(sect), entry[1:6]);
		}else{
			target <- rbind(target, cbind(t(sect), entry[1:6]));
		}
		return(target);
	}
	# transect_desc
	for(i in 1:length(data)){
		if(length(data[[i]][!is.na(data[[i]])]) > 0){
			for(j in 1:nrow(data[[i]])){
				clos <- closest(c(data[[i]][j,3], data[[i]][[j,2]]), cbind(transect_desc[,5], transect_desc[,4]));
				result <- insert(result, data[[i]][j,], clos);
			}
		} 
	}


	colnames(result) <- c("section", "subsectio", "date", "latitude", "longitude", "elevation", "contemporary", "historical");
	rownames(result) <- 1:nrow(result);


	# result
	filename
	write.csv(result,filename, row.names=FALSE)
}



## work start exportCSV(paths to all files, name of result file);
for(i in 1:length(paths)){
	exportCSV(list.files(paths[i], pattern = "*.csv", full.names = TRUE), paste(gsub(" ", "_",dirs[i]), ".csv", sep=""));
}
