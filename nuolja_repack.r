# INSTRUCTIONS
# 1. Put nuolja_repack.r and transect_description.csv into the parent directory of the 'Snow Data YYYY' directories 
# 2. Run Rscript nuolja_repack.r file
# 3. The script will create one file for each child directory.

# colnames = c("plot", "subplot", "proj_factor", "transect_dist", "id", "date", "latitude", "longitude", "elevation", "contemporary", "historical");
colnames = c("plot", "subplot", "proj_factor", "id", "date", "latitude", "longitude", "elevation", "contemporary", "historical");


## Install geosphere package if not already install on client
list.of.packages <- c("geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(geosphere);

source("nuolja_help.r")

## Build paths for the directories
paths <- list.dirs(getwd(),full.names = TRUE)[-1]
dirs <- list.dirs(getwd(), full.names = FALSE)[-1];


# Generate list of distance between start point and each pole
# also it sets order of column to "plot, subplot "
# transect_desc = data.frame(transect_desc);

section = function(x){
	# print(typeof(x))
	prc = NA;
	for(i in 1:(nrow(transect_desc)-1)){
		p0 = as.numeric(transect_desc[i,3]);
		p1 = as.numeric(transect_desc[i+1,3]);
		if((p0 - x) <= 0.0 && (p1 - x) >= 0.0){
			return(c(transect_desc[i,c(1,2)]));
		} 
	}
	return(transect_desc[78,c(1,2)])
}

## this writes to csv file based on name called at the end of the document
exportCSV <- function(filenames, filename){
	if(length(filenames)==0) return(NULL)
	# Parsing the data from the file 
	data = lapply(filenames, function(x){
			      ## filter to Historical perspective
			      historical = function(y){
				      tmp = tolower(as.vector(y));
				      if(tmp %in% "so" || tmp %in% "s"|| tmp %in% "os") tmp = c("s");
				      if(tmp %in% "r") tmp = c("o");
				      return(tmp);
			      }
			      ## filter out 
			      concurrent = function(z){
				      tmp = tolower(as.vector(z));
				      if(tmp %in% "r") tmp = "o";
				      if(tmp %in% "x") tmp = prev; 
				      return(tmp);
			      }
			      # filter out none numerical used for coordinates who use the E & N notation.
			      filter = function(y){
				      return(lapply(y, function(x) as.double(gsub("[^0-9\\.]", "", y))));
			      }

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
		proj = projection(e[1:3])
		d = proj[1]
		# print(proj)
		# print(section(d))
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

	# print(result)

	colnames(result) <- colnames; 
	# result = rbind(result, transect_desc);

	# result = result[order(delist(result$proj_factor)),];
	rownames(result) <- 1:nrow(result);


	# print(result$proj_factor)
	# print(order(delist(result$proj_factor)))
	# print(order(as.numeric(result$proj_factor)))
	# result
	# print(result[1:30,])	
	# print(result[-(1:180),])	
	# plot(result[(result[,1] %in% 10),5:7])
	tmp = result;
	tmp = result[(result[,1] > 8),];
	tmp = result[(result[,1] < 11),];
	# tmp = result[(result[,1] == 10),];
	# print(tmp)
	# tmp = tmp[tmp$date == "2019-05-10",]
	# print(tmp)
	if(nrow(tmp)>1)plot(tmp[c(3,5)],type="p", pch=as.character(tmp$historical))
	# if(nrow(tmp)>1)plot(tmp[c(6,7)],type="b", pch=as.character(tmp$plot))
	write.csv(result,filename, row.names=FALSE)
}

## work start exportCSV(paths to all files, name of result file) for each directory path
# it will grab all csv files in the directories 
for(i in 1:length(paths)){
	exportCSV(list.files(paths[i], pattern = "*.csv", full.names = TRUE), paste(gsub(" ", "_",dirs[i]), ".csv", sep=""));
}


print("nuolja_repack.r : DONE : CSV files built");
