## Script will create sub files for all Snow_Data_YYYY.csv in current directory
## name convention will be YYYY_[historical/contemporary]_[plot/subplot].csv



library(data.table)
library(geosphere);

## formating header and finding matching files in current directory to pattern "Snow_Data_*"
filenames <- list.files(getwd(), pattern = "*.csv", TRUE);
filenames <- subset(filenames, grepl("Snow_Data_*", filenames));
filenames <- gsub(".csv", "", filenames);
filenames <- gsub("[^0-9\\.]", "", filenames);
files <- list.files(getwd(), pattern = "*.csv", full.names = TRUE);
files <- subset(files, grepl("Snow_Data_*", files));

transect_desc <- read.csv(file="transect_description.csv")[,1:6];
transect_desc <- data.matrix(transect_desc);
#############

delist = function(y){
	return(vapply(y, paste, collapse = ', ', character(1L)));
}


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

percSeg <- function(ds){
	# print(ds)
	ds = unlist(ds)
	result = c();
	for(i in 2:(length(ds))){
		result = c(result, signif(abs(ds[i]-ds[i-1])/(max(ds)-min(ds)), 10));
	}
	return(result)	
}
# print(percSeg(c(10,5,3,2,0)))
# print(percSeg(c(15,10,8,7,5)))
# print(percSeg(c(10,5,0)))

sumPerc = function(m){
	default = c(m[nrow(m),3], m[nrow(m),2])
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
		if(sum(percent) > 1){
			# print("............")
			# print(percent)
			# print(sum(percent))
			# print(m)
	
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
	# print(entries[1:10,])
	# print(vs[1:10,])
	plots = unique(max(entries[,1]))
	type = colnames(entries)[1];
	days = unique(entries$date);
	entries = data.table(entries);
	entries = entries[order(entries$proj_factor)]
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
			# print("START")
			sub_e = getSubE(entries, p, day);
			
			v0 = vs[vs[,1] == p,]
			v1 = vs[vs[,1] == (p+1),]
			if(is.vector(v0)){
				d0 = distm(v0[2:3], vs[1,2:3])
			}else{
				d0 = distm(v0[1,2:3], vs[1,2:3])
			}
			# print("MID")
			# print(v1)
			# print(p)
			if(is.vector(v1)){
				d1 = distm(v1[2:3], vs[1,2:3])
			}else{
				n = nrow(v1);
				# print(n)
				if(n==0){
					d1 = distm(v1[n,2:3], vs[1:2:3])
				}else{
					d1 = distm(v1[n,2:3], vs[1,2:3])
				}
			}
			# print(d0)
			# print(d1)
			if(is.na(sub_e[1,2])){
				dn = cbind(d0, d1)
			}else{
				dn = cbind(cbind(d0, t(sub_e[,2])), d1)
			}
			# print(sub_e)
			# print(dn)
			# print(v0)
			# print(v1)
			percent = percSeg(dn)


			# print(default)
			m = rbind(default, sub_e[,8:9]);
			# print(percent)		
			if(sum(percent)>1.01){
				print("------------------------")
				# print(sub_e)	
				# print(dn)
				# print(nrow(m))
				# print(length(percent))
				# print(m)
				# print(sum(percent))
				# print(sub_e)
				# print(sub_e[,2])
				# print(percent)
				# print(dn)
			}

			m = cbind(percent, m);
			# print(m)
			perc = sumPerc(m)
			# print(perc$contemporary)	
			

			# print("END")
			
			
			contemporary = rbind(contemporary, c(day, vs[p,1], perc$contemporary));
			historical = rbind(historical, c(day, vs[p,1], perc$historical));
			default = perc$default;	
			# print("-------")
			# print(default)
			# print("<<<<<<<<<<<")
		}
	}
	# result = data.table(result);
	plotname = colnames(entries)[1];
	colnames(contemporary) <- c("date", plotname, keyset$contemporary);
	colnames(historical) <- c("date", plotname, keyset$historical);
	# print(colnames(result))
	# print(contemporary[1:20,])
	write = function(data, file, t){
		name = paste(file, paste(type, ".csv", sep=""), sep="");
		print(paste("Write to : ", name))
		write.csv(data, name, row.names=FALSE);
	}


	write(contemporary, paste(filename, "_contemporary_", sep=""), type);
	write(historical, paste(filename, "_historical_", sep=""), type);
	print(paste("DONE -", type))
}

buildCSV <- function(dataset, filename){
	subplot = subCalc(dataset[-1], filename, transect_desc[,c(1,5,4)]);
	plotcoord = matrix(,nrow=0,ncol=3)
	for(i in 1:20){
				  tmp = transect_desc[transect_desc[,3]==i,]
				  # print(tmp)
				  res = tmp[1,c(3,5,4)]
				res = tmp[1, c(3,5,4)];
				  if(i==20){
					res = rbind(res, c(i+1, tmp[nrow(tmp), c(5,4)]))
				  }
				  plotcoord = rbind(plotcoord, res)
	}
	plot = subCalc(dataset[-2], filename, plotcoord);
}

for(i in 1:length(files)){
	buildCSV(data[[i]], filenames[i]);
}
# print(data[[1]])
# print(subCalc(data[[1]], "historical", "plot"), nrow=300)
