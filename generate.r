## Script will create sub files for all Snow_Data_YYYY.csv in current directory
## name convention will be YYYY_[historical/contemporary]_[plot/subplot].csv


# Description of what the script does:
# 1. Read all Snow_Data_YYYY.csv files in current directory
# 2. For each file, it will create a sub file for each plot and subplot
# 3. For each plot and subplot, it will create a sub file for each day
# 4. For each day, it will create a sub file for each segment
# 5. For each segment, it will create a sub file for each segment


library(data.table)
library(geosphere);
source("helper.r");

## formating header and finding matching files in current directory to pattern "Snow_Data_*"
filenames <- list.files(paste(getwd(), "/repack", sep=""), pattern = "*.csv", TRUE);
filenames <- subset(filenames, grepl("Snow_Data_*", filenames));
filenames <- gsub(".csv", "", filenames);
# filenames <- gsub("[^0-9\\.]", "", filenames);
files <- list.files(paste(getwd(), "/repack", sep=""), pattern = "*.csv", full.names = TRUE);
files <- subset(files, grepl("Snow_Data_*", files));
#############

data <- lapply(files, function(x) read.delim(x, header=TRUE, sep=","));

## Define key sets historical are binary meanwhile contemporary are quaternary.
keyset  <- list(contemporary = c("s", "so", "o", "os"), historical = c("s", "o"))

match <- function(entries, key){
	entries = vapply(entries, paste, collapse = ", ", character(1L));
	result = vapply(lapply(entries, function(x){
				       return(identical(as.vector(x), key));
}), paste, collapse = ', ', character(1L))
	return(result)
}

percSeg <- function(ds){
	ds = unlist(ds)
	result = c();
	for(i in 2:(length(ds))){
		result = c(result, abs(ds[i]-ds[i-1])/(max(ds)-min(ds)));
	}
	return(result)	
}

sumPerc = function(m){
	default = c(m[nrow(m),2], m[nrow(m),3])
	resolve = function(keys, mat, values){
		percent = c()
		for(k in keys){
			value = values[mat %in% k,1];
			if(nrow(value)==0){
				percent = c(percent, 0);
			}else{
				percent = c(percent, sum(value)); 
			}
		}
		return(percent)
	}
	return(list(
		    contemporary=resolve(keyset$contemporary, m$contemporary, m[,1]), 
		    historical=resolve(keyset$historical, m$historical, m[,1]),
		    default = default
		    ))
}


matrixCalc <- function(entries, filename){
	days = unique(entries$date);
	entries = data.table(entries);
	entries = entries[order(-entries$proj_factor)]
	
	historical = data.frame();
	contemporary = data.frame();

	top = max(as.integer(round(transect_desc[,3])+1));
	subplot = rep(1, top);
	for(i in 1:(nrow(transect_desc)-1)){
		tmp = transect_desc[i,]
		index=as.integer(round(tmp[3]))+1;
		length = 1:index;
		subplot[-length] = rep(tmp[2], top-length(length));	
	}
	subplot = subplot;
	# print(top)
	# print(subplot)	
	for(day in days){
		mat_entry = entries[date %in% day]
		h_e = rep("o", top);
		c_e = rep("o", top);
		

		for(i in 1:nrow(mat_entry)){
			# print(i)
			tmp = mat_entry[i,]
			index=as.integer(round(tmp$proj_factor))+1;
			h_value = delist(tmp$historical);
			c_value = delist(tmp$contemporary);
			length = 1:index;
			h_e[length] = rep(h_value, length(length));
			c_e[length] = rep(c_value, length(length));
		}
		historical = rbind(historical, t(h_e));
		contemporary = rbind(contemporary, t(c_e))
	}
	historical = rbind(t(subplot), historical);
	contemporary = rbind(t(subplot), contemporary);
	name = getName(filename, "_mat_historical");
	print(paste("Write to : ", name))
	row.names(historical) <- c("subplots", as.character(days));
	colnames(historical) <- 1:top;
	write.csv(historical, name);
	name = getName(filename, "_mat_contemporary")
	print(paste("Write to : ", name))
	row.names(contemporary) <- c("subplots", as.character(days));
	colnames(contemporary) <- 1:top;
	write.csv(contemporary, name);
	return(list(historical, contemporary))
}

subCalc <- function(entries, filename, vs){
	plots = unique(max(entries[,1]))
	type = colnames(entries)[1];
	days = unique(entries$date);
	entries = data.table(entries);
	entries = entries[order(-entries$proj_factor)]
	getSubE = function(e,x,y){
		e = e[as.vector(e[,1] == x),];
		return(e[date==y]);
	} 
	contemporary = matrix(,nrow=0,ncol=6) 
	historical = matrix(,nrow=0,ncol=4);


	for(day in days){
		doy = format(as.Date(day), "%j");
		default = data.table("o", "o") 
		colnames(default) <- c("contemporary", "historical")


		for(p in 1:plots){
			sub_e = getSubE(entries, p, day);

			## vs are sorted from lowest to highest
			## take in reverse order
			d1 = vs[vs[,1] == p,]
			d0 = vs[vs[,1] == (p+1),]
			# print(d0)
			# print(d1)
			if(is.na(sub_e[1,2])){
				dn = cbind(d0[2], d1[2])
			}else{
				dn = cbind(cbind(d0[2], t(sub_e[,2])), d1[2])
			}
			percent = percSeg(dn)
			m = rbind(default, sub_e[,8:9]);
			m = cbind(percent, m);

			perc = sumPerc(m)
			contemporary = rbind(contemporary, c(doy, vs[p,1], perc$contemporary));
			historical = rbind(historical, c(doy, vs[p,1], perc$historical));
			default = perc$default;	
		}
	}


	plotname = colnames(entries)[1];
	colnames(contemporary) <- c("DOY", plotname, keyset$contemporary);
	colnames(historical) <- c("DOY", plotname, keyset$historical);
	write = function(data, file, t){
		name = getName(file, t);
		print(paste("Write to : ", name))
		write.csv(data, name, row.names=FALSE);
	}

	write(contemporary, paste(filename, "_contemporary_", sep=""), type);
	write(historical, paste(filename, "_historical_", sep=""), type);
	print(paste("DONE -", type))
	return(list(contemporary = contemporary, historical = historical));
}



##############################################
## Build csv file and write into out directory
##############################################
buildCSV <- function(dataset, filename){
	print(paste("Processing -", filename))
	subplot = subCalc(dataset[-1], filename, transect_desc[,c(2,3)]);
	plotcoord = matrix(,nrow=0,ncol=2)
	for(i in 1:20){
		tmp = transect_desc[transect_desc[,1]==i,]
		res = c(i, min(tmp[,3]));
		if(i==20){
			res = rbind(res, c(i+1, max(tmp[,3])))
		}
		plotcoord = rbind(plotcoord, res)
	}
	plot = subCalc(dataset[-2], filename, plotcoord);

	return(matrixCalc(dataset[-1], filename));

}

for(i in 1:length(files)){
	temp = buildCSV(data[[i]], filenames[i]);
}


print(paste("Completed Processing :", files))

warnings()
