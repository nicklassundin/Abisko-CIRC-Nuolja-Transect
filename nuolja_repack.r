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



projection = function(x){
	t0 = as.vector(c(transect_desc[1,5], transect_desc[1,4]))
	t1 = as.vector(c(transect_desc[nrow(transect_desc),5], transect_desc[nrow(transect_desc),4]));
	x = as.double(x);
	dist2Line = dist2Line(c(x[2], x[1]), rbind(t0,t1));
	return(distm(dist2Line[2:3], t0));
}

subplot_dist = t(apply(transect_desc, 1, function(x){
	return(c(x[c(3,1)], distm(x[c(5,4)], transect_desc[1,c(5,4)])));	
}))
# subplot_dist
section = function(x){
	# print(x)
	prc = NA;
	for(i in 1:(nrow(subplot_dist)-1)){
		p0 = subplot_dist[i,3];
		p1 = subplot_dist[i+1,3];
		if(p0 - x <= 0.00001 && p1 - x >= 0.00001){
			return(c(subplot_dist[i,c(1,2)]));
		} 
	}
	return(subplot_dist[78,c(1,2)])
}

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
		proj = projection(e[1:2])
		d = proj[1,1]
		return(c(section(d), d));
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
				# print(clos)	
				result <- insert(result, data[[i]][j,], clos);
			}
		} 
	}

	## naming the columns for the return file
	colnames(result) <- c("plot", "subplot", "proj_factor", "id", "date", "latitude", "longitude", "elevation", "contemporary", "historical");
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
