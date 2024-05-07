library(data.table);
library(geosphere);

# Keysets definitions
keyset <- list(contemporary = c("s", "so", "o", "os"), historical = c("s", "o"));

# colnames = c("plot", "subplot", "proj_factor", "transect_dist", "id", "date", "latitude", "longitude", "elevation", "contemporary", "historical");
colnames = c("plot", "subplot", "proj_factor", "id", "date", "latitude", "longitude", "elevation", "contemporary", "historical");

delist = function(y){
	return(vapply(y, paste, collapse = ', ', character(1L)));
}

# TODO description
section = function(df, x){
	prc = NA;
	for(i in 1:(nrow(df)-1)){
		p0 = as.numeric(df[i,3]);
		p1 = as.numeric(df[i+1,3]);
		if((p0 - x) <= 0.0 && (p1 - x) >= 0.0){
			return(c(df[i,c(1,2)]));
		} 
	}
	return(df[78,c(1,2)])
}

# Brute forces point form from file fineds closest points
# assumes that transect is never turning >45 degrees
closest <- function(e, df){
	# evaluating distance between three different point undtil closest two point are found and return interval number (subsection) including what section the subsection belongs to 
	proj = projection(df, e[1:3])
	d = proj[1]
	return(c(section(df, d), d));
};
projection = function(transect_desc, x){
	t0 = as.vector(c(transect_desc[1,5],transect_desc[1,4]))
	t1 = as.vector(c(transect_desc[nrow(transect_desc),5], transect_desc[nrow(transect_desc),4]));
	x = as.double(x);
	dist2Line = dist2Line(c(x[2], x[1]), rbind(t0,t1));
	dist = distm(dist2Line[2:3], t0);

	res = c(dist[1], dist2Line[1])
	return(res)
}

## load transect_description.csv used as reference file for the transect line
loadTransectDescription = function(path = paste(getwd(), "/descriptions", sep=""), pattern = "transect_description.csv") {
	td_file <- list.files(path,full.names = TRUE)
	td_file <- td_file[grepl(pattern, td_file)];
	transect_desc <- read.csv(file=td_file)[,1:6];
	transect_desc <- data.matrix(transect_desc);


	transect_desc = t(apply(transect_desc, 1, function(x){
				return(c(x[c(3,1)], projection(transect_desc, x[4:6])[1], x[4:6]));
	}))
	
	return(transect_desc)
}
getName = function(sourceFileName, specification) {
	return(paste(paste("out/", paste(sourceFileName, specification, sep="_"), sep=""), ".csv", sep=""))
}
