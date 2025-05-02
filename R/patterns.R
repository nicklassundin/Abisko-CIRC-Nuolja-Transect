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
					    validateField = function(line) {
						    # check that all patterns are present
						    current_species <- str_extract(line, self$ACCEPTABLE_CODES$Species)
						    codes <- self$ACCEPTABLE_CODES[!is.na(current_species), "Code"]
						    # return only species not NA
						    current_species <- current_species[!is.na(current_species)]
						    valid_species <- str_detect(line, current_species) 
						    if (length(current_species) == 0) {
							    valid_species <- FALSE
						    }

						    date <- str_extract(line, self$PATTERNS$date)
						    date <- date[!is.na(date)]
						    subplot <- str_extract(line, self$PATTERNS$subplot)
						    subplot <- subplot[!is.na(subplot)]
						    # split codes ',' sent '|' 

						    list(
							 species = valid_species,
							 date = str_detect(line, date), 
							 subplot = str_detect(line, subplot),
							 code = str_detect(line, paste(codes, collapse = "|"))
							 )
					    }))
