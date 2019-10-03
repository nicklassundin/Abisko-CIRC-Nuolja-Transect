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


genPercen <- function(entries, p, startvalue=list(historical="o", contemporary="o")){
	# print(entries)
	# print("start percent call")
	name = colnames(entries);
	if(nrow(entries) == 0){
		entries = data.table(NA, NA, NA, NA, 0, 0, 0, contemporary=startvalue$contemporary, historical=startvalue$historical)	
	}
	# print(entries)
	distance = function(entries){
		if(nrow(entries)<1) return(NA);
		points = cbind(entries[,6], entries[,5])
		p0 = c(transect_desc[p,5], transect_desc[p,4]);
		p1 = c(transect_desc[p+1,5], transect_desc[p+1,4]);
		p_m = p0 - (p0-p1)/2;
		d01 = distm(p0, p1)
		result = c(1)
		for(i in 1:nrow(points)){
			pn = points[i,]
			d0 = distm(pn, p0);
			d1 = distm(pn, p1);
			# print("-----------------")
			# print(pn)
			# print(p0)
			# print(d0)
			# print(p1)
			# print(d1)
				
			pn0 = dist2Line(pn, rbind(p0,p1));
			# print(pn0)
			perc = pn0[1]/d01;

			if(abs(perc[1,1]) > 1){
				perc = 0;
			}
			result[i] = result[i] - perc;
			result = c(result, perc);
		}
		return(cbind(entries, result[-1]))
	}
	percent = distance(entries);
	# print(percent)
	values = c();
	for(k in keyset$contemporary){
		value = 0;
		if(k %in% startvalue$contemporary) value = value + (1 - sum(percent[,10]));
		# filt = filter[filter %in% k]
		
		for(i in 1:nrow(percent)){
			if(percent[i,]$contemporary %in% k){
				value = value + percent[i,10];
			}
		}
		values = c(values, as.double(value))
	}
	cont = values;	
	
	values = c();
	for(k in keyset$historical){
		value = 0;
		if(k %in% startvalue$historical) value = value + (1 - sum(percent[,10]));
		# filt = filter[filter %in% k]
		
		for(i in 1:nrow(percent)){
			if(percent[i,]$historical %in% k){
				value = value + percent[i,10];
			}
		}
		values = c(values, as.double(value))
	}
	hist = values;
	result = list(historical=hist, contemporary=cont, default=list(
								       historical=percent$historical[nrow(percent)],
								       contemporary = percent$contemporary[nrow(percent)]
								       ));
	# print(result)
	return(result);
}
# genPercen(data.table(data[[1]])[plot==20], keyset$historical)
# genPercen(data.table(data[[1]])[plot==20], keyset$contemporary)
# data.table(data[[1]])[plot==20][,"historical"]
# genPercen(data.table(data[[1]])[plot==20][,"historical"], keyset$historical)
# genPercen(data.table(data[[1]])[plot==20][,"contemporary"], keyset$contemporary)


subCalc <- function(entries, filename){
	days = unique(entries$date);
	entries = data.table(entries);
	
	getSubE = function(e,x,y) return(e[date==y][ subplot==x]);
	contemporary = matrix(,nrow=0,ncol=7) 
	historical = matrix(,nrow=0,ncol=5);
	for(day in days){
		default = list(historical="o", contemporary="o");
		for(p in 1:78){
			sub_e = getSubE(entries, p, day);
			# print(sub_e)
			perc = genPercen(sub_e, p, default);
			contemporary = rbind(contemporary, c(day, transect_desc[p,3], p, perc$contemporary));
			historical = rbind(historical, c(day, p, perc$historical));
			default = perc$default;	
		}
	}
	# result = data.table(result);
	colnames(contemporary) <- c("date", "plot", "subplot", keyset$contemporary);
	colnames(historical) <- c("date", "plot", "subplot", keyset$historical);
	# print(colnames(result))
	write.csv(contemporary, paste(filename, "_contemporary_subplot.csv", sep=""), row.names=FALSE);
	write.csv(historical, paste(filename, "_historical_subplot.csv", sep=""), row.names=FALSE);
}

buildCSV <- function(dataset, filename){
	# print(filename)
	# write.csv(subCalc(dataset, "contemporary"), paste(filename, "_contemporary_plot.csv", sep=""), row.names=FALSE);
	subplot = subCalc(dataset, filename);
	# write.csv(subCalc(dataset, "historical"), paste(filename, "_historical_subplot.csv", sep=""), row.names=FALSE)
}

for(i in 1:length(files)){
	buildCSV(data[[i]], filenames[i]);
}
print("Done");

# print(data[[1]])
# print(subCalc(data[[1]], "historical", "plot"), nrow=300)
