# Patterns for validation
prefix_pattern <- "^NS-"
datetime_pattern <- "\\d{8}-\\d{3}"
latitude_pattern <- "\\d+\\.\\d{6,9}N"
longitude_pattern <- "\\d+\\.\\d{6,9}E"
elevation_pattern <- "\\d+\\.\\d+"
obs_code_pattern <- "[osOS]{1,2}$"

# Patterns for lenient validation
lenient_latitude_pattern <- "\\d+\\.\\d+N"
lenient_longitude_pattern <- "\\d+\\.\\d+E"

STRICT_PATTERN <- paste(prefix_pattern, datetime_pattern, latitude_pattern, longitude_pattern, elevation_pattern, obs_code_pattern, sep = "[ , ]+")
# STRICT_PATTERN <- "^NS-\\d{8}-\\d{3}[ , ]\\d+\\.\\d+N[ , ]\\d+\\.\\d+E[, ]\\d+\\.\\d+[, ][osOS]{1,2}$"
LENIENT_PATTERN <- paste(lenient_latitude_pattern, lenient_longitude_pattern, elevation_pattern, obs_code_pattern, sep = "[ , ]")
OLD_LENIENT_PATTERN <- "^[ , ]\\d+\\.\\d+N[ , ]\\d+\\.\\d+E[, ]\\d+\\.\\d+[, ][osOS]{1,2}$"


# Snow Structure with patterns
snow_structure <- list(
  id = prefix_pattern,
  datetime = datetime_pattern,
  latitude = latitude_pattern,
  longitude = longitude_pattern,
  elevation = elevation_pattern,
  obs_code = obs_code_pattern
)
# Snow Structure with patterns
SNOW_PATTERNS <- list(
	STRICT_PATTERN = STRICT_PATTERN,
	LENIENT_PATTERN = LENIENT_PATTERN,
	STRUCT = list(
		id = prefix_pattern,
		datetime = datetime_pattern,
		latitude = latitude_pattern,
		longitude = longitude_pattern,
		elevation = elevation_pattern,
		obs_code = obs_code_pattern
	)
)

# Phenology Structure with patterns
phenology_structure <- list(
	species = "[A-Za-z]+(?: [A-Za-z-]+)?",
	date = "\\d{4}-\\d{2}-\\d{2}",
	subplot = "\\d{1,2} to \\d{1,2}",
	code = "*"
)
# Phenology Structure with patterns
PHENO_PATTERNS <- list(
	LENIENT_PATTERN = paste(phenology_structure$species, phenology_structure$date, phenology_structure$subplot, phenology_structure$code, sep = "[ , ]"),
	STRUCT = phenology_structure
)
