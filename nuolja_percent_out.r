## Script will create sub files for all Snow_Data_YYYY.csv in current directory
## name convention will be YYYY_[historical/contemporary]_[plot/subplot].csv



library(data.table)
library(geosphere);
source("nuolja_help.r");

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
	result = vapply(lapply(entries, function(x){
				       return(identical(as.vector(x), key));
}), paste, collapse = ', ', character(1L))
	return(result)
}

percSeg <- function(ds){
	# print("percSeg <<<<<<<<<<")
	ds = unlist(ds)
	# print(ds)
	result = c();
	# print(max(ds)-min(ds))
	for(i in 2:(length(ds))){
		# if(ds[i]-ds[i-1]<0){
		# print("percSeg <<<<<<<<<<")
			# print(ds[length(ds)] - ds[1])
			# print(ds[i])
			# print(ds)
			# print(ds[i-1])
			# print(ds[i]-ds[i-1])
		# }
		result = c(result, abs(ds[i]-ds[i-1])/(max(ds)-min(ds)));
	}
	# print(result)
	return(result)	
}

sumPerc = function(m){
	# print("<<<<<<<<<<<<      >>>>>>>>>>>>")

	default = c(m[nrow(m),2], m[nrow(m),3])
	# print(default)
	resolve = function(keys, mat, values){
		percent = c()
		# print(keys)
		# print(values)
		for(k in keys){
			value = values[mat %in% k,1];
			# print(k)
			# print(mat)
			# print(value)
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




subCalc <- function(entries, filename, vs){
	# print(vs)
	plots = unique(max(entries[,1]))
	type = colnames(entries)[1];
	days = unique(entries$date);
	entries = data.table(entries);
	# print(entries[order(entries$proj_factor)]);
	entries = entries[order(-entries$proj_factor)]
	# print(entries)
	getSubE = function(e,x,y){
		e = e[as.vector(e[,1] == x),];
		return(e[date==y]);
	} 
	contemporary = matrix(,nrow=0,ncol=6) 
	historical = matrix(,nrow=0,ncol=4);
	for(day in days){
		default = data.table("o", "o") 
		colnames(default) <- c("contemporary", "historical")
		for(p in 1:plots){
			# print(entries)
			sub_e = getSubE(entries, p, day);
			# print(p)
			# print(sub_e)

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
			# print(dn)
			percent = percSeg(dn)
			# print("<<<<<<<<< >>>>>>>>>")
			# print(dn)
			# print(percent)
			# print(default)
			m = rbind(default, sub_e[,8:9]);
			m = cbind(percent, m);
			# print(m)

			perc = sumPerc(m)
			contemporary = rbind(contemporary, c(day, vs[p,1], perc$contemporary));
			historical = rbind(historical, c(day, vs[p,1], perc$historical));
			default = perc$default;	
		}
	}
	plotname = colnames(entries)[1];
	colnames(contemporary) <- c("date", plotname, keyset$contemporary);
	colnames(historical) <- c("date", plotname, keyset$historical);
	write = function(data, file, t){
		name = paste(file, paste(type, ".csv", sep=""), sep="");
		print(paste("Write to : ", name))
		write.csv(data, name, row.names=FALSE);
	}
	


	write(contemporary, paste(filename, "_contemporary_", sep=""), type);
	write(historical, paste(filename, "_historical_", sep=""), type);
	print(paste("DONE -", type))
	# if(type == "plot")print(historical[historical[ , 2] == 11, ])
	return(list(contemporary = contemporary, historical = historical));
}


buildCSV <- function(dataset, filename){
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

}

for(i in 1:length(files)){
	buildCSV(data[[i]], filenames[i]);
}
