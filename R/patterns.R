library(R6)
library(dplyr)
library(stringr)

# SNOW VALIDATOR CLASS
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

# PHENOLOGY VALIDATOR CLASS
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
							    group_by(Code) %>%
							    summarise(Species = paste(Species, collapse = "|")) %>%
							    ungroup()

						    phenology_structures <- list()
						    phenology_species_patterns <- list()

					    },

					    validate = function(line) {
						    # check if species is in the acceptable codes
						    species <- str_extract(line, self$ACCEPTABLE_CODES$Species)
						    if (is.na(species)) {
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
						    # remove \"
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

						    # check that all patterns are present
						    current_species <- str_extract(line$species, self$ACCEPTABLE_CODES$Species)
						    codes <- self$ACCEPTABLE_CODES[!is.na(current_species), "Code"]
						    # TODO check if "+" is valid temporarily add "+"
						    # append "+" as character code to the end of the codes
						    codes <- paste(codes, "\\+", sep = ",")
							

						    # if codes is not empty
						    if (length(codes) != 0) {
							    # split on comma and ';'
							    codes <- str_split(codes, ",|;", simplify = TRUE)
						    }
						    # unique codes values
						    codes <- unique(codes)
						    # return only species not NA
						    current_species <- current_species[!is.na(current_species)]
						    valid_species <- str_detect(line$species, current_species) 
						    if (length(current_species) == 0) {
							    valid_species <- FALSE
						    }

						    date <- str_extract(line$date, self$PATTERNS$date)
						    date <- date[!is.na(date)]
						    valid_date <- str_detect(line$date, date)
						    if (length(date) == 0) {

						    }
						    subplot <- str_extract(line$subplot, self$PATTERNS$subplot)
						    subplot <- subplot[!is.na(subplot)]
						    valid_subplot <- str_detect(line$subplot, subplot)
						    if (length(subplot) == 0) {
							    valid_subplot <- FALSE
						    }
						    code <- str_extract(line$code, codes)
						    code <- code[!is.na(code)]
						    valid_code <- str_detect(line$code, codes)
						    # combine all codes boolean
						    valid_code <- any(valid_code)
						    if (length(code) == 0) {
							    code <- FALSE
						    }
						    code_lenient <- str_extract(tolower(line$code), tolower(codes))
						    code_lenient <- code_lenient[!is.na(code_lenient)]
						    valid_code_len <- str_detect(tolower(line$code), tolower(codes))
						    valid_code_len <- any(valid_code_len)
						    # check if code is in the acceptable codes
						    if (length(code_lenient) == 0) {
							    code_lenient <- FALSE
						    }


						    if (valid_code_len) {
						    } else if(valid_species) {
							    # print("invalid code")
							    # print(strLine)
							    # print(line$species)
							    # print(line$code)
							    # print(tolower(codes))
						    }
						    list(
							 species = valid_species,
							 date = valid_date,
							 subplot = valid_subplot,
							 # code = valid_code
							 code = valid_code_len
							)
					    }))
