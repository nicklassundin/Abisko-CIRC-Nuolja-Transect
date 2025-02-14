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
