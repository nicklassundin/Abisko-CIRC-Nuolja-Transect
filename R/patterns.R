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
				       #' @field STRICT_PATTERN Regular expression patterns for strict validation'
				       #' @description Regular expression patterns for strict validation
				       STRICT_PATTERN = NULL,
				       #' @field LENIENT_PATTERN Regular expression patterns for lenient validation
				       #' @description Regular expression patterns for strict validation
				       LENIENT_PATTERN = NULL,
				       #' @field PATTERNS Regular expression patterns for validating individual fields
				       #' @description Regular expression patterns for validating individual fields
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
				       #' @description Initialize the SnowValidator class
				       #' @return None
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
				       #' @description Validate a line of snow data
				       #' @param line A string representing a line of snow data
				       #' @return A list with lenient and strict validation results
				       validate = function(line) {
					       fields <- self$validateField(line)
					       list(
						    strict = str_detect(line, self$STRICT_PATTERN),
						    lenient = str_detect(line, self$LENIENT_PATTERN),
						    fields = fields
					       )
				       },
				       #' @description Validate individual fields in a line of snow data
				       #' @param line A string representing a line of snow data
				       #' @return A list indicating the validity of each field
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




OBSERVED_SPECIES_LIST_FILE_NAME = "descriptions/Nuolja Master Documents/Plant List and Phenology Codes Master.xlsx"
ACCEPTABLE_CODES_FILE_NAME = "descriptions/Nuolja Master Documents/Accepted_PhenoCodes_Species_CURRENT.csv"

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
					    #' @field LENIENT_PATTERN Regular expression patterns for lenient validation
					    #' @description Regular expression patterns for strict validation
					    LENIENT_PATTERN = NULL,
					    #' @field STRUCT Regular expression patterns for strict validation'
					    #' @description Regular expression patterns for strict validation
					    STRUCT = NULL,
					    #' @field PATTERNS Regular expression patterns for validating individual fields
					    #' @description Regular expression patterns for validating individual fields
					    PATTERNS = list(
							    date = "\\d{4}-\\d{2}-\\d{2}",
							    subplot = "\\d{1,2} to \\d{1,2}"
							    ),
					    #' @field ACCEPTABLE_CODES Data frame of acceptable phenology codes and species
					    #' @description Data frame of acceptable phenology codes and species
					    ACCEPTABLE_CODES = NULL,
					    #' @field PREVIOUS_SPECIES List of previously observed species
					    #' @description List of previously observed species
					    PREVIOUS_SPECIES = NULL,
					
					    #' @description Initialize the PhenologyValidator class
					    #' @param csv_path Path to the CSV file containing acceptable phenology codes and species
					    #' @return None
					    initialize = function(csv_path = "descriptions/Nuolja Master Documents/Accepted_PhenoCodes_Species_CURRENT.csv") {
						    acceptable_codes <- read.csv(csv_path, stringsAsFactors = FALSE, sep = ";")
						    acceptable_codes$Code <- apply(acceptable_codes[, -1], 1, function(x) paste(x, collapse = ","))
						    self$ACCEPTABLE_CODES <- acceptable_codes %>%
							    group_by(Code, Species);
						    # remove trailing spaces end of string
						    self$ACCEPTABLE_CODES$Species <- gsub("\\s+$", "", self$ACCEPTABLE_CODES$Species)
						    # change codes into array split on "," and ";"
						    self$ACCEPTABLE_CODES$Code <- strsplit(as.character(self$ACCEPTABLE_CODES$Code), ",|;")
						    # remove leading spaces
						    self$ACCEPTABLE_CODES$Code <- lapply(self$ACCEPTABLE_CODES$Code, function(x) gsub("^\\s+", "", x))
						    # handle '+' for regex
						    self$ACCEPTABLE_CODES$Code <- lapply(self$ACCEPTABLE_CODES$Code, function(x) gsub("\\+", "\\\\+", x))
						    # remove dublicate codes
						    self$ACCEPTABLE_CODES <- self$ACCEPTABLE_CODES %>%
							    group_by(Species) %>%
							    summarise(Code = list(unique(unlist(Code)))) %>%
							    ungroup()
						    phenology_structures <- list()
						    phenology_species_patterns <- list()

						    # Species Previously Observed
						    # load second sheet in .xlsx file
						    species <- read.xlsx(OBSERVED_SPECIES_LIST_FILE_NAME, sheet = 2, colNames = TRUE) 
						    # combine all columns
						    species <- unlist(species)
						    # remove NA
						    species <- species[!is.na(species)]
						    # unique species
						    species <- unique(species)
						    self$PREVIOUS_SPECIES <- species;
						    # write header on log/missing.phen.log
						    file.create("log/missing.phen.log")
						    # write header Species Acceptable Code missing in Master Document
						    cat("Species in Acceptable Code Species that are missing in Master Species List", file = "log/missing.phen.log")
						    missing <- self$ACCEPTABLE_CODES[!self$ACCEPTABLE_CODES$Species %in% species,]$Species
						    # paste together with \n
						    missing <- paste(missing, collapse = "\n")
						    # write missing species to file
						    if (length(missing) > 0) {
							    cat("\n", file = "log/missing.phen.log", append = TRUE)
							    cat(missing, file = "log/missing.phen.log", append = TRUE)
						    }
						    # write --- breakline and new header for missing species in 
						    cat("\n", file = "log/missing.phen.log", append = TRUE)
						    cat("---------------------", file = "log/missing.phen.log", append = TRUE)
						    cat("\n", file = "log/missing.phen.log", append = TRUE)
						    # reverse check
						    cat("Species in Master Document that are missing in Acceptable Code Species", file = "log/missing.phen.log", append = TRUE)
						    # check if species in master document is in acceptable codes
						    missing <- species[!species %in% self$ACCEPTABLE_CODES$Species]
						    # paste together with \n
						    missing <- paste(missing, collapse = "\n")
						    # write missing species to file
						    if (length(missing) > 0) {
							    cat("\n", file = "log/missing.phen.log", append = TRUE)
							    cat(missing, file = "log/missing.phen.log", append = TRUE)
						    }

						    cat("\n", file = "log/missing.phen.log", append = TRUE)
						    cat("---------------------", file = "log/missing.phen.log", append = TRUE)
						    cat("\n", file = "log/missing.phen.log", append = TRUE)
						    cat("Species in Data that are missing in Acceptable Code", file = "log/missing.phen.log", append = TRUE)
					    },
					    #' @description Validate a line of phenology data
					    #' @param line A string representing a line of phenology data
					    #' @return A list with lenient validation results and field-wise validation
					    validate = function(line) {
						    valid <- self$validateField(line)
						    return(list(
								lenient = all(unlist(valid)),
								fields = valid
								))
					    },
					    #' @field missing_species List of species that were found in data but are not in the acceptable codes
					    #' @description List of species that were found in data but are not in the acceptable codes
					    missing_species = c(),
					    #' @description Validate individual fields in a line of phenology data
					    #' @param strLine A string representing a line of phenology data
					    #' @return A list indicating the validity of each field
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
							    # read file
							    # check if species is in the file
							    if (!line$species %in% self$missing_species) {
								    # write species at next line
								    cat("\n", file = "log/missing.phen.log", append = TRUE)
								    cat(line$species, file = "log/missing.phen.log", append = TRUE)
								    self$missing_species <- c(self$missing_species, line$species)
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
						    # remove NA from valid_code
						    valid_code <- valid_code[!is.na(valid_code)]
						    valid_code <- any(valid_code)
						    if (!valid_code) {
							    return (list(
									 species = valid_species,
									 date = valid_date,
									 subplot = valid_subplot,
									 code = valid_code
									 ))
						    }

						    list(
							 species = valid_species,
							 date = valid_date,
							 subplot = valid_subplot,
							 # code = valid_code
							 code = valid_code
						    )
						    # }, error = {
						    # return (list(
						    # RUN_TIME_ERROR = FALSE
						    # ))
						    # })
					    }))
