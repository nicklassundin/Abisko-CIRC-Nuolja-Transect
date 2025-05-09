library(R6)
library(dplyr)
library(stringr)

#' Snow Validator Class'
#' inherits from R6Class
#' @description This class is used to validate snow data.
#' @param csv_path path to the csv file with acceptable codes
#' @return a list with lenient and strict validation
#' @examples
#' snow_validator <- SnowValidator$new()
#' snow_validator$validate("NS-20230101-001 60.123456N 15.123456E 100.0 os")
#' @export
#' @importFrom dplyr %>%
#' @importFrom stringr str_detect str_extract
#' @importFrom R6 R6Class
#' @importFrom readr read_csv
#' @importFrom tidyr separate
#' @importFrom stringr str_split
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
SnowValidator <- R6Class("SnowValidator",
			 public = list(
				       STRICT_PATTERN = NULL,
				       LENIENT_PATTERN = NULL,
				       PATTERNS = list(
						       prefix_pattern = "^NS-",
						       datetime_pattern = "\\d{8}-\\d{3}",
						       latitude_pattern = "\\d+\\.\\d{6,9}N",
						       longitude_pattern = "\\d+\\.\\d{6,9}E",
						       elevation_pattern = "\\d+\\.\\d+",
						       obs_code_pattern = "[osOS]{1,2}$",
						       lenient_latitude_pattern = "\\d+\\.\\d+N",
						       lenient_longitude_pattern = "\\d+\\.\\d+E"
						       ),
				       initialize = function() {
					       # self$STRICT_PATTERN <- list(prefix_pattern, datetime_pattern, latitude_pattern,
					       # longitude_pattern, elevation_pattern, obs_code_pattern,
					       # sep = "[ , ]+")
					       # self$LENIENT_PATTERN <- paste(lenient_latitude_pattern, lenient_longitude_pattern,
					       # elevation_pattern, obs_code_pattern, sep = "[ , ]+")
					       self$STRICT_PATTERN <- paste(self$PATTERNS$prefix_pattern, 
									    self$PATTERNS$datetime_pattern,
									    self$PATTERNS$latitude_pattern,
									    self$PATTERNS$longitude_pattern,
									    self$PATTERNS$elevation_pattern,
									    self$PATTERNS$obs_code_pattern,
									    sep = "[ , ]+")
					       self$LENIENT_PATTERN <- paste(self$PATTERNS$lenient_latitude_pattern,
									     self$PATTERNS$lenient_longitude_pattern,
									     self$PATTERNS$elevation_pattern,
									     self$PATTERNS$obs_code_pattern,
									     sep = "[ , ]+")

				       },
				       validate = function(line) {
					       list(
						    strict = str_detect(line, self$STRICT_PATTERN),
						    lenient = str_detect(line, self$LENIENT_PATTERN)
					       )
				       },
				       validateField = function(line) {
					       # check that all patterns are present
					       list(
						    prefix = str_detect(line, self$PATTERNS$prefix_pattern),
						    datetime = str_detect(line, self$PATTERNS$datetime_pattern),
						    latitude = str_detect(line, self$PATTERNS$latitude_pattern),
						    longitude = str_detect(line, self$PATTERNS$longitude_pattern),
						    elevation = str_detect(line, self$PATTERNS$elevation_pattern),
						    obs_code = str_detect(line, self$PATTERNS$obs_code_pattern)
					       )
				       }

			 )
)

#' Phenology Validator Class
#' inherits from R6Class
#' @description This class is used to validate phenology data.
#' @param csv_path path to the csv file with acceptable codes
#' @return a list with lenient and strict validation
#' @examples
#' phenology_validator <- PhenologyValidator$new()
#' phenology_validator$validate("species, date, subplot, code")
#' @export
#' @importFrom dplyr %>%
#' @importFrom stringr str_detect str_extract str_split
#' @importFrom R6 R6Class
#' @importFrom readr read_csv
#' @importFrom tidyr separate
PhenologyValidator <- R6Class("PhenologyValidator",
			      public = list(
					    LENIENT_PATTERN = NULL,
					    STRUCT = NULL,
					    PATTERNS = list(
							    date = "\\d{4}-\\d{2}-\\d{2}",
							    subplot = "\\d{1,2} to \\d{1,2}"
							    ),
					    ACCEPTABLE_CODES = NULL,

					    initialize = function(csv_path = "descriptions/Nuolja Master Documents/Accepted_PhenoCodes_Species_CURRENT.csv") {
						    acceptable_codes <- read.csv(csv_path, stringsAsFactors = FALSE, sep = ";")
						    acceptable_codes$Code <- apply(acceptable_codes[, -1], 1, function(x) paste(x, collapse = ","))
						    self$ACCEPTABLE_CODES <- acceptable_codes %>%
							    group_by(Code, Species);
						    # remove trailing spaces end of string
						    self$ACCEPTABLE_CODES$Species <- gsub("\\s+$", "", self$ACCEPTABLE_CODES$Species)
						    # change codes into array split on "," and ";"
						    self$ACCEPTABLE_CODES$Code <- strsplit(as.character(self$ACCEPTABLE_CODES$Code), ",|;")
						    # remove dublicate codes
						    self$ACCEPTABLE_CODES <- self$ACCEPTABLE_CODES %>%
							    group_by(Species) %>%
							    summarise(Code = list(unique(unlist(Code)))) %>%
							    ungroup()
						    phenology_structures <- list()
						    phenology_species_patterns <- list()

					    },

					    validate = function(line) {
						    # check if species is in the acceptable codes
						    species <- str_extract(line, self$ACCEPTABLE_CODES$Species)
						    species <- species[!is.na(species)]
						    if (is.na(species) || length(species) == 0) {
							    return(list(lenient = FALSE) # no species found
							    )
						    }
						    # TODO just return all
						    return (list(
								 lenient = TRUE
								 ))

						    # check if code is in the acceptable codes
						    code <- str_extract(line, self$ACCEPTABLE_CODES$Code)
						    if (is.na(code)) {
							    return(list(lenient = FALSE) # no code found
							    )
						    }
						    # check combination of species, code, date and subplot
						    combination <- paste(species, code, self$PATTERNS$date, self$PATTERNS$subplot, sep = "[ , ]+")
						    list(
							 lenient = str_detect(line, combination),
						    )

					    },
					    validateField = function(strLine) {
						    # tryCatch({
							    # # remove \"
							    line <- gsub("\"", "", strLine)
							    line <- gsub('"', "", line)
							    # devide on comma into list with colnames
							    line <- str_split(line, ",", simplify = TRUE)
							    line <- list(
									 species = line[1],
									 date = line[2],
									 subplot = line[3],
									 code = line[4]
							    )
							    # line$species is in accpetable

							    valid_species <- line$species %in% self$ACCEPTABLE_CODES$Species
						    if (!valid_species) {
							   # load log/missing.phen.txt
							    if (!file.exists("log/missing.phen.txt")) {
								    file.create("log/missing.phen.txt")
							    }
							    # read file
							    missing_phen <- readLines("log/missing.phen.txt")
							    # check if species is in the file
							    if (!line$species %in% missing_phen) {
								    # write species at next line
								    cat("\n", file = "log/missing.phen.txt", append = TRUE)
								    cat(line$species, file = "log/missing.phen.txt", append = TRUE)
							    }
						    }
							    if (!valid_species) {
								    return (list(
										 species = valid_species
										 ))
							    }
							    current_species <- line$species
							    codes <- self$ACCEPTABLE_CODES[self$ACCEPTABLE_CODES$Species == current_species,]$Code[[1]]

							    valid_date <- str_detect(line$date, self$PATTERNS$date)
							    if (!valid_date) {
								    return (list(
										 species = valid_species,
										 date = valid_date
										 ))
							    }
							    valid_subplot <- str_detect(line$subplot, self$PATTERNS$subplot)
							    if (!valid_subplot) {
								    valid_subplot <- FALSE
								    return (list(
										 species = valid_species,
										 date = valid_date,
										 subplot = valid_subplot
										 ))
							    }

							    valid_code <- str_detect(line$code, codes)
							    # combine all codes boolean
							    valid_code <- any(valid_code)
							    if (!valid_code) {
								    # code <- FALSE
								    # return (list(
								    # species = valid_species,
								    # date = valid_date,
								    # subplot = valid_subplot,
								    # code = code
								    # ))
							    }
							    valid_code_len <- str_detect(tolower(line$code), tolower(codes))
							    valid_code_len <- any(valid_code_len)
							    # check if code is in the acceptable codes
							    if (!valid_code_len) {
								    return (list(
										 species = valid_species,
										 date = valid_date,
										 subplot = valid_subplot,
										 code = valid_code_len
										 ))
							    }


							    list(
								 species = valid_species,
								 date = valid_date,
								 subplot = valid_subplot,
								 # code = valid_code
								 code = valid_code_len
							    )
						    # }, error = {
							    # return (list(
									 # RUN_TIME_ERROR = FALSE
									 # ))
						    # })
					    }))
