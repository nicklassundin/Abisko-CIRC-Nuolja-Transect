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

# Read Acceptable codes from description/Nuolja Master Document/Acceptable_PhenoCodes_Species_CURRENT.csv
acceptable_codes <- read.csv("descriptions/Nuolja Master Documents/Accepted_PhenoCodes_Species_CURRENT.csv", stringsAsFactors = FALSE, sep = ";")
# merge all columns but Species into one column as a string with comma separated values
acceptable_codes$Code <- apply(acceptable_codes[, -1], 1, function(x) paste(x, collapse = ","))

# group by Code and merge Species like name | name2
acceptable_codes <- acceptable_codes %>%
	group_by(Code) %>%
	summarise(Species = paste(Species, collapse = "|")) %>%
	ungroup()

# Phenology Structure with patterns
# for each species in the acceptable_codes$Species, add a pattern

if (!dir.exists("out/patterns")) {
	dir.create("out/patterns")
}

# if files exist return them
if (file.exists("out/patterns/phenology_structures.rds") && file.exists("out/patterns/phenology_species_patterns.rds")) {
	phenology_structures <- readRDS("out/patterns/phenology_structures.rds")
	phenology_species_patterns <- readRDS("out/patterns/phenology_species_patterns.rds")
} else {
	# create empty lists
	phenology_structures <- list()
	phenology_species_patterns <- list()
	for (i in 1:nrow(acceptable_codes)) {
		current_species <- acceptable_codes$Species[i]
		# mutate acceptable_codes, Code = strsplit(Code, ","))

		codes <- acceptable_codes[acceptable_codes$Species == current_species, "Code"]
		# remove spaces
		codes <- gsub(" ", "", codes)
		codes <- unlist(strsplit(codes, ","))

		phenology_structure <- list(
					    # species = "[A-Za-z]+(?: [A-Za-z-]+)?",
					    species = current_species, 
					    date = "\\d{4}-\\d{2}-\\d{2}",
					    subplot = "\\d{1,2} to \\d{1,2}",
					    # code = "*"
					    code = paste(codes, collapse = "|")

		)
		# species = "[A-Za-z]+(?: [A-Za-z-]+)?",
		# species = paste(acceptable_codes$Species, collapse = "|"),
		# date = "\\d{4}-\\d{2}-\\d{2}",
		# subplot = "\\d{1,2} to \\d{1,2}",
		# code = "*"
		phenology_structure$species <- acceptable_codes$Species[i]
		phenology_structure$code <- paste(acceptable_codes$Code[i], collapse = "|")
		phenology_structures[[i]] <- phenology_structures
		phenology_species_patterns[[i]] <- paste(phenology_structure$species, phenology_structure$date, phenology_structure$subplot, phenology_structure$code, sep = "[ , ]")
	}
}
# save phenology_structures and phenology_species_patterns to a file
saveRDS(phenology_structures, file = "out/patterns/phenology_structures.rds")
saveRDS(phenology_species_patterns, file = "out/patterns/phenology_species_patterns.rds")
# Phenology Structure with patterns
PHENO_PATTERNS <- list(
		       # LENIENT_PATTERN = paste(phenology_structure$species, phenology_structure$date, phenology_structure$subplot, phenology_structure$code, sep = "[ , ]"),
		       LENIENT_PATTERN = paste(phenology_species_patterns, collapse = "|"),
		       STRUCT = phenology_structures
)
