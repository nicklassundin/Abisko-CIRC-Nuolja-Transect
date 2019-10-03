# INSTRUCTIONS
# 1. Put nuolja_repack.r and transect_description.csv into the parent directory of the 'Snow Data YYYY' directories 
# 2. Run Rscript nuolja_repack.r file
# 3. The script will create one file for each child directory.




## Install geosphere package if not already install on client
list.of.packages <- c("geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(geosphere);

## load transect_description.csv used as reference file for the transect line
transect_desc <- read.csv(file="transect_description.csv")[,1:6];
transect_desc <- data.matrix(transect_desc);
###

## Build paths for the directories
paths <- list.dirs(getwd(),full.names = TRUE)[-1]
dirs <- list.dirs(getwd(), full.names = FALSE)[-1];

## this writes to csv file based on name called at the end of the document
exportCSV <- function(filenames, filename){
	if(length(filenames)==0) return(NULL)
	# Parsing the data from the file 
	data = lapply(filenames, function(x){
				## filter to Historical perspective
			       historical = function(y){
				       if(y %in% "so" || y %in% "s"|| y %in% "os") return(c("s"));
				       if(y %in% "o") return(c("o"));
			       }
			       ## filter out 
			       concurrent = function(z){
				       tmp = tolower(as.vector(z));
				       if(tmp %in% "r") tmp = "o";
				       return(tmp);
			       }
			       # filter out none numerical used for coordinates who use the E & N notation.
			       filter = function(y){
				       return(lapply(y, function(x) as.double(gsub("[^0-9\\.]", "", y))));
			       }

			       delist = function(y){
				       return(vapply(y, paste, collapse = ', ', character(1L)));
			       }
				## Active reading, filtering and end unlist (removes every column beond 7)
			       entries = read.delim(x, header=FALSE, sep=",")[,1:5]
			       if(dim(entries)[2] < 5) return(NA);
			       dates = lapply(as.character(entries[,1]), function(x) return(strsplit(x,"-")[[1]][2]));
			       dates = as.Date(delist(dates), "%Y%m%d");
			       entries[,2] = delist(lapply(entries[,2], filter));
			       entries[,3] = delist(lapply(entries[,3], filter));
			       entries[,5] = delist(lapply(entries[,5], concurrent));
			       hist = data.matrix(lapply(entries[,5], historical));
			       # unlisting to be able to bind it to 'entries'
			       hist <- delist(hist)
			       entries = cbind(entries, hist);
				entries = cbind(entries[,1], cbind(dates, entries[,-1]));
			       # return full structure
			       return(entries);
} 
	);

	# Brute forces point form from file fineds closest points
	# assumes that transect is never turning >45 degrees
	closest <- function(e, chart, i=1){
		# evaluating distance between three different point undtil closest two point are found and return interval number (subsection) including what section the subsection belongs to 
		sect <- function(n){
			return(c(transect_desc[n,3], transect_desc[n,1]));
		}
		distance = function(e0, e1){
			# print(e0)
			# print(e1)
			# reverse latitude and longitude because distm takes longitude, latitude
			d = dist2Line(c(e0[2], e0[1]), cbind(e1[,2],e1[,1]));
			# d = sqrt(d^2 + (e0[3]-e1[3])^2);
			return(d)
		}
		e = as.double(e);
		d1 = distance(e[1:2], chart[i:(i+1),1:2]);
		d01 = distm(c(chart[i,2], chart[i,1]), c(chart[i+1,2], chart[i+1,1]))
		# print(d1[1])
		# print(d01)
		# print(d1[1] < d01)
		# fsdfsdf	
		if(d1[1] < d01){
				return(sect(i))
		}
		if(nrow(chart) > i+2){
			return(closest(e, chart, i+1));
		}else{
				return(sect(i+1));
		}

	};

	# initating result data.frame
	result <- data.frame();

	## inserts new entry to target and return target
	insert <- function(target, entry, sect){
		if(is.null(target)){
			target <- cbind(t(sect), entry);
		}else{
			target <- rbind(target, cbind(t(sect), entry));
		}
		return(target);
	}
	# Accumulative build result row by row, with insert(,,);
	for(i in 1:length(data)){
		if(length(data[[i]][!is.na(data[[i]])]) > 0){
			for(j in 1:nrow(data[[i]])){
				clos <- closest(data[[i]][j,3:5], transect_desc[,4:6]);
				
				result <- insert(result, data[[i]][j,], clos);
			}
		} 
	}

	## naming the columns for the return file
	colnames(result) <- c("plot", "subplot", "id", "date", "latitude", "longitude", "elevation", "contemporary", "historical");
	rownames(result) <- 1:nrow(result);


	# result
	filename
	write.csv(result,filename, row.names=FALSE)
}

## work start exportCSV(paths to all files, name of result file) for each directory path
# it will grab all csv files in the directories 
for(i in 1:length(paths)){
	exportCSV(list.files(paths[i], pattern = "*.csv", full.names = TRUE), paste(gsub(" ", "_",dirs[i]), ".csv", sep=""));
}

print("nuolja_repack.r : DONE : CSV files built");
