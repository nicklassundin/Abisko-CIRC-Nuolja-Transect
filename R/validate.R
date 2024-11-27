
# Patterns for validation
prefix_pattern <- "^NS-"
datetime_pattern <- "\\d{8}-\\d{3}"
latitude_pattern <- "\\d+\\.\\d{6,9}N"
lenient_latitude_pattern <- "\\d+\\.\\d+N"
longitude_pattern <- "\\d+\\.\\d{6,9}E"
lenient_longitude_pattern <- "\\d+\\.\\d+E"
elevation_pattern <- "\\d+\\.\\d+"
obs_code_pattern <- "[osOS]{1,2}$"
STRICT_PATTERN <- paste(prefix_pattern, datetime_pattern, latitude_pattern, longitude_pattern, elevation_pattern, obs_code_pattern, sep = "[ , ]+")
# STRICT_PATTERN <- "^NS-\\d{8}-\\d{3}[ , ]\\d+\\.\\d+N[ , ]\\d+\\.\\d+E[, ]\\d+\\.\\d+[, ][osOS]{1,2}$"
LENIENT_PATTERN <- paste(lenient_latitude_pattern, lenient_longitude_pattern, elevation_pattern, obs_code_pattern, sep = "[ , ]")
OLD_LENIENT_PATTERN <- "^[ , ]\\d+\\.\\d+N[ , ]\\d+\\.\\d+E[, ]\\d+\\.\\d+[, ][osOS]{1,2}$"
# print(LENIENT_PATTERN)
# print(OLD_LENIENT_PATTERN)

#' Validate a Single Line Against a Specific Format
#'
#' This function checks whether a given line matches a specified format pattern. 
#' The pattern enforces a structure that starts with 'NS-', followed by a date-time 
#' stamp, latitude, longitude, and an observation code.
#'
#' @param line A character vector representing a single line to be validated.
#' @param file (Optional) The name of the file being processed (default is NA).
#' @param line_number (Optional) The line number within the file (default is NA).
#' @param log_file (Optional) A character vector specifying a file to which validation 
#'        error messages should be logged. If NULL, logging is disabled.
#' 
#' @return A logical value indicating whether the line matches the specified format pattern.
#' 
#' @examples
#' validateLine("NS-20220510-001 68.37261219N 18.69783E 1195.186 O")
#' validateLine("NS-20231015-034 70.12345678N 20.54321E 1500.000 os")
#'
#' @export
validateLine <- function(line, file = NA, line_number = NA, log_file = NULL) {
	# Define the regular expression pattern
	pattern <- LENIENT_PATTERN
	# Check if the line matches the pattern
	validation_result <- grepl(pattern, line)
	# If line is invalid and a log_file is provided, log errors
	if (!validation_result && !is.null(log_file)) {
		printValidationError(line, file, line_number, log_file)
	}
	if (!validation_result) {
		print(line, validation_result)
		# wait for user input
		readLines(file("stdin"), 1)
	}
	return(validation_result)

}

#' Count and Log Errors for Each File and Type of Error
#'
#' This function aggregates the count of each type of validation error
#' encountered in a file and logs the summary to a separate error count file.
#'
#' @param error_list A list of error messages generated during validation.
#' @param file The name of the file being processed.
#' @param count_log_file (Optional) A character vector specifying a file where error 
#'        counts are logged. Default is "log/error_count_summary.txt".
#' 
#' @return NULL (This function is called for its side effect of logging error counts.)
#'
#' @examples
#' logErrorCounts(c("Error: Missing latitude format.", "Error: Missing latitude format."), "data.txt")
#'
#' @export
logErrorCounts <- function(error_list, file, count_log_file = "log/error_count_summary.txt") {
	# Create a table to count occurrences of each error message
	error_count <- table(unlist(error_list))

	# Prepare the log entry
	log_entry <- paste0("Error Count Summary for File: ", file, "\n")
	log_entry <- paste0(log_entry, "----------------------------------------\n")
	for (error in names(error_count)) {
		log_entry <- paste0(log_entry, error, ": ", error_count[[error]], "\n")

	}
	log_entry <- paste0(log_entry, "\n\n")

	# Write the error summary to the log file
	cat(log_entry, file = count_log_file, append = TRUE)

}

#' Validate Line and Print Field-Specific Errors
#'
#' This function validates a line against a specified format pattern and prints/logs
#' detailed errors for each field that does not match the expected format.
#'
#' @param line A character vector representing the line to be validated.
#' @param file (Optional) The name of the file containing the line (default is NA).
#' @param line_number (Optional) The line number within the file (default is NA).
#' @param log_file (Optional) A character vector specifying a file where error messages 
#'        are logged. If NULL, messages are only printed to the console.
#' 
#' @return NULL (This function is called for its side effect of printing/logging messages.)
#'
#' @examples
#' printValidationError ("NS-20220510-001 68.37261219N 18.69783 1195.186 O", "data.txt", 5, "log/validation.txt")
#' 
#' @export
printValidationError <- function(line, file = NA, line_number = NA, log_file = NULL) {
	errors <- c()
	# Check prefix
	if (!grepl(prefix_pattern, line)) {
		errors <- c(errors, "Error: Line does not start with 'NS-' prefix.")
	}

	# Check date-time format
	if (!grepl(datetime_pattern, line)) {
		errors <- c(errors, "Error: Missing or incorrect date-time format (YYYYMMDD-NNN).")
	}

	# Check latitude
	if (!grepl(latitude_pattern, line)) {
		errors <- c(errors, "Error: Missing or incorrect latitude format (e.g., 68.37261219N).")
	}

	# Check longitude
	if (!grepl(longitude_pattern, line)) {
		errors <- c(errors, "Error: Missing or incorrect longitude format (e.g., 18.69783E).")
	}

	# Check elevation
	if (!grepl(elevation_pattern, line)) {
		errors <- c(errors, "Error: Missing or incorrect elevation format (e.g., 1195.186).")
	}

	# Check observation code
	if (!grepl(obs_code_pattern, line)) {
		errors <- c(errors, "Error: Missing or incorrect observation code format (e.g., O or OS).")
	}

	# Format error messages
	if (length(errors) > 0) {
		error_message <- paste(errors, collapse = "\n")
		formatted_message <- paste0(
					    "Validation Errors:\n",
					    "File: ", ifelse(is.na(file), "N/A", file), "\n",
					    "Line Number: ", ifelse(is.na(line_number), "N/A", line_number), "\n",
					    "Line: ", line, "\n",
					    error_message, "\n"
		)

		# Log error message to file if specified
		if (!is.null(log_file)) {
			cat(formatted_message, file = log_file, append = TRUE)
		}
	}
	return(errors)
}


#' Validate All Lines in a File Against a Specific Format
#'
#' This function reads a file line by line and validates each line against a specified 
#' format pattern. It prints and optionally logs error messages for lines that do 
#' not match the expected format.
#'
#' @param file_path A character vector representing the path to the file to be validated.
#' 
#' @return A logical vector where each element indicates whether the corresponding line 
#'         in the file matches the specified format pattern.
#' 
#' @examples
#' validateFile("data.txt")
#' 
#' @export
validateFile <- function(file_path, silent = FALSE) {
	log_file <- "log/validation.txt"
	error_list <- list()

	# Read the file line by line
	lines <- readLines(file_path)
	validation_results <- logical(length(lines))

	# create progress bar
	if (!silent) {
		pb <- txtProgressBar(min = 0, max = length(lines), style = 3)
	}
	# Validate each line and collect errors
	for (i in seq_along(lines)) {
		# update progress bar
		if (!silent) setTxtProgressBar(pb, i)
		errors <- printValidationError(
						   lines[i], 
						   file = file_path, 
						   line_number = i, 
						   log_file = log_file)
		# append errors to error_list
		error_list <- c(error_list, errors)

		validation_results[i] <- validateLine(lines[i], file = file_path, line_number = i, log_file = log_file)

	}
	# close progress bar
	if (!silent) {
		close(pb)
	}

	# Log error counts
	logErrorCounts(error_list, file_path)

	# Return validation results (TRUE for valid lines, FALSE for invalid)
	return(validation_results)

}
