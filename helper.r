library(data.table);
library(geosphere);


## load transect_description.csv used as reference file for the transect line

td_file <- list.files(getwd(),full.names = TRUE)
td_file <- td_file[grepl("transect_description.csv", td_file)];

transect_desc <- read.csv(file=td_file)[,1:6];
transect_desc <- data.matrix(transect_desc);
###

delist = function(y){
	return(vapply(y, paste, collapse = ', ', character(1L)));
}


projection = function(x){
	t0 = as.vector(c(transect_desc[1,5], transect_desc[1,4]))
	t1 = as.vector(c(transect_desc[nrow(transect_desc),5], transect_desc[nrow(transect_desc),    4]));
	x = as.double(x);
	dist2Line = dist2Line(c(x[2], x[1]), rbind(t0,t1));
	dist = distm(dist2Line[2:3], t0);

	h0 = transect_desc[1,6];
	h1 = transect_desc[nrow(transect_desc),6];
	# print("<<<<<<<<<<<<     >>>>>>>")
	# if need to consider height
	# ratio = dist/distm(t0,t1);
	# h = ration*(h1-h0);
 	# dist = cbind(dist, dist2Line[1])
	
	res = c(dist[1], dist2Line[1])
	# print(dist)
	return(res)
}

transect_desc = t(apply(transect_desc, 1, function(x){
				return(c(x[c(3,1)], projection(x[4:6])[1], x[4:6]));
}))

getName = function(sourceFileName, specification) {
	return(paste(paste("out/", paste(sourceFileName, specification, sep=""), sep=""), ".csv", sep=""))
}
