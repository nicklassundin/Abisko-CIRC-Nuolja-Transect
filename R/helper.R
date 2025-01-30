library(data.table)
library(geosphere)

# Keysets definitions
keyset <- list(contemporary = c("s", "so", "o", "os"), historical = c("s", "o"))

# Column names for the data
colnames <- c("plot", "subplot", "proj_factor", "id", "date", "latitude", "longitude", "elevation", "contemporary", "historical")

#' @title Delist
#' @description Convert a list of character vectors into a single character vector with concatenated elements.
#' @param y A list of character vectors.
#' @return A character vector with concatenated elements.
#' @export
delist <- function(y) {
	return(vapply(y, paste, collapse = ', ', character(1L)))

}

#' @title createDir
#' @description Create a new sub directory inside the main path.
#' @param subdir A character string representing the name of the sub directory.
#' @export
createDir <- function(subdir){
	if(!file.exists(subdir)){
		# create a new sub directory inside
        	# the main path
		dir.create(file.path(getwd(), subdir))
	}
}

#' @title exportCSV
#' @description Export the data from the input files to a CSV file.
#' @param filenames A character vector representing the paths to the input files.
#' @param filename A character string representing the name of the output file.
#' @return A list containing the data frame and the output file name.
exportCSV <- function(filenames, filename){
	if(length(filenames)==0) return(NULL)
	# Parsing the data from the file
	
	# entries = read.delim(filenames[1], header=FALSE, sep=",")[,1:5]
	lapply(filenames, validateFile);

	data = lapply(filenames, readFile);
	# Accumulative build result row by row, with insert(,,);
	result <- dataframeBuilder(data);
	return(list(result=result, filename=filename));	
}

#' @title Section
#' @description Find the section of the dataframe corresponding to a given projection factor.
#' @param df A data frame containing the data.
#' @param x A numeric value representing the projection factor.
#' @return A numeric vector representing the section of the dataframe.
#' @export
section <- function(df, x) {
	prc <- NA
	for (i in 1:(nrow(df) - 1)) {
		p0 <- as.numeric(df[i, 3])
		p1 <- as.numeric(df[i + 1, 3])
		if ((p0 - x) <= 0.0 && (p1 - x) >= 0.0) {
			return(c(df[i, c(1, 2)]))

		}

	}
	return(df[78, c(1, 2)])

}

#' @title Closest
#' @description Find the closest point in the dataframe to a given point, and return the corresponding section and distance.
#' @param e A numeric vector representing the point coordinates.
#' @param df A data frame containing the reference points.
#' @return A numeric vector representing the section and distance.
#' @export
closest <- function(e, df) {
	proj <- projection(df, e[1:3])
	d <- proj[1]
	return(c(section(df, d), d))

}

#' @title Projection
#' @description Calculate the projection of a point onto the transect line.
#' @param transect_desc A data frame containing the transect description.
#' @param x A numeric vector representing the point coordinates.
#' @return A numeric vector representing the distance along the transect and the perpendicular distance to the line.
#' @export
projection <- function(transect_desc, x) {
	t0 <- as.vector(c(transect_desc[1, 5], transect_desc[1, 4]))
	t1 <- as.vector(c(transect_desc[nrow(transect_desc), 5], transect_desc[nrow(transect_desc), 4]))
	x <- as.double(x)
	dist2Line <- dist2Line(c(x[2], x[1]), rbind(t0, t1))
	dist <- distm(dist2Line[2:3], t0)

	res <- c(dist[1], dist2Line[1])
	return(res)

}

#' @title Load Transect Description
#' @description Load the transect description from a CSV file.
#' @param path A character string representing the path to the directory containing the transect description file. Default is the "descriptions" directory in the current working directory.
#' @param pattern A character string representing the pattern to match the transect description file. Default is "transect_description.csv".
#' @return A data matrix containing the transect description.
#' @export
loadTransectDescription <- function(path = paste(getwd(), "/descriptions", sep=""), pattern = "transect_description.csv") {
	td_file <- list.files(path, full.names = TRUE)
	td_file <- td_file[grepl(pattern, td_file)]
	transect_desc <- read.csv(file = td_file)[, 1:6]
	transect_desc <- data.matrix(transect_desc)

	transect_desc <- t(apply(transect_desc, 1, function(x) {
					 return(c(x[c(3, 1)], projection(transect_desc, x[4:6])[1], x[4:6]))

}))

	return(transect_desc)

}

#' @title Get Name
#' @description Generate a file name based on the source file name and a specification.
#' @param sourceFileName A character string representing the source file name.
#' @param specification A character string representing the specification to append to the file name.
#' @return A character string representing the generated file name.
#' @export
getName <- function(sourceFileName, specification) {
	return(paste(paste("out/", paste(sourceFileName, specification, sep="_"), sep=""), ".csv", sep=""))

}
