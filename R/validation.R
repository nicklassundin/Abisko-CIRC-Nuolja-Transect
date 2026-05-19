source("R/patterns.R")

#' Create a Backup of the Log File
#' 
#' This function creates a backup of the specified log file by renaming it with a
#' timestamp. If a backup file already exists, it is removed before creating the new
#' backup.
#'
#' @param log_file A character vector specifying the name of the log file.
#'
#' @return NULL (This function is called for its side effect of creating a backup.)
#'
#' @examples
#' create_backup("error.log")
create_backup <- function(log_file) {
	# remove log/missing.phen.txt
	if (file.exists(log_file)) {
		file.remove(log_file)
	}
	log_backup_file = paste0("log/backup.", log_file)
	log_file = paste0("log/", log_file)
	if (file.exists(log_backup_file)) {
		file.remove(log_backup_file)
	}
	if (file.exists(log_file)) {
		file.rename(log_file, log_backup_file)
	}
}

#' Create the File Structure for Logging
#'
#' This function creates a backup of the specified log file and initializes the
#' necessary log files for error and warning messages. It also creates a directory
#' for the log files if it does not already exist.
#'
#' @param log_file A character vector specifying the name of the log file.
#'
#' @return NULL (This function is called for its side effect of creating log files.)
#'
#' @examples
#' create_file_structure("error.log")
create_file_structure <- function(log_file) {
	# create log directory if it does not exist
	if (!dir.exists("log")) {
		dir.create("log")
	}
	log_error_file = paste0("error.", log_file)
	create_backup(log_error_file)
	warnings_file = paste0("warnings.", log_file)
	create_backup(warnings_file)
	error_count_file = paste0("count.", log_file)
	create_backup(error_count_file)
	error_missing_file = paste0("missing.", log_file)
	create_backup(error_missing_file)
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
logErrorCounts <- function(error_list, file, count_log_file) {
	# Create a table to count occurrences of each error message
	error_count <- table(unlist(error_list))

	# create file if it does not exist
	if (!file.exists(count_log_file)) {
		file.create(count_log_file)
	}

	# Prepare the log entry
	log_entry <- paste0("Error Count Summary for File: ", file, "\n")
	# Add total number of lines in file
	numLines <- length(readLines(file))
	log_entry <- paste0(log_entry, "Total Lines: ", numLines, "\n")
	log_entry <- paste0(log_entry, "----------------------------------------\n")
	for (error in names(error_count)) {
		log_entry <- paste0(log_entry, error, ": ", error_count[[error]], "\n")
	}
	log_entry <- paste0(log_entry, "\n\n")

	# Write the error summary to the log file
	if (length(error_count) != 0) {
		cat(log_entry, file = count_log_file, append = TRUE)
	}
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
#' printValidationError ("NS-20220510-001 68.37261219N 18.69783 1195.186 O", "data.txt", 5, "log/validation.txt", "log/warnings.txt")
#' 
#' @export
printValidationError <- function(line, validation, file = NA, line_number = NA, log_file = NULL) {
	warnings_file = paste0("log/warnings.", log_file)
	errors <- c()
	# Check prefix
	# iterate over key and value pairs in PATTERNS$STRUCT
	# check if validator is NULL
	validField <- validation$fields
	for (key in names(validField)) {
		if (!validField[[key]]) {
			errors <- c(errors, paste("Message: Missing or incorrect", key, "format."))
		}
	}

	# Format error messages
	if (length(errors) > 0) {
		error_message <- paste(errors, collapse = "\n")
		critical <- !validation$lenient
		formatted_message <- paste0(
					    "----------------------------------------\n",
					    "Validation Errors:\n",
					    "File: ", ifelse(is.na(file), "N/A", file), "\n",
					    "Line Number: ", ifelse(is.na(line_number), "N/A", line_number), "\n",
					    "Line: ", line, "\n",
					    error_message, "\n",
					    "critical error:", critical, "\n")

		# Log error message to file if specified
		log_file = paste0("log/error.", log_file)
		if (!is.null(log_file)) {
			if (critical){
				cat(formatted_message, file = log_file, append = TRUE)
			}else{
				cat(formatted_message, file = warnings_file, append = TRUE)
			}
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
validateFile <- function(file_path, silent = FALSE, validator, log_file="log/error.log", head=FALSE) {
	# Create the file structure
	error_list <- list()
	# Read the file line by line
	tryCatch(
		 lines <- readLines(file_path),
		 error = function(e) {
			 if (!silent) {
				 cat("Error reading file:", file_path, "\n")
			 }
			 return(NULL)
		 }
	)
	if(head) {
		lines = lines[-1]
	}
	# return if file is empty
	if (length(lines) == 0) {
		if (!silent) {
			cat("File is empty:", file_path, "\n")
		}
		return(NULL)
	}
	validation_results <- logical(length(lines))
	# Print file path
	total <- length(lines)
	start_time <- Sys.time()
	for (i in seq_along(lines)) {
		validation <- validator$validate(lines[i])
		errors <- printValidationError(
					       lines[i], 
					       file = file_path, 
					       line_number = i, 
					       log_file = log_file,
					       validation = validation
		)
		# append errors to error_list
		error_list <- c(error_list, errors)

		 
		validation_results[i] <- validation$lenient
	}

	# Log error counts
	count_log_file = paste0("log/count.", log_file)
	logErrorCounts(error_list, file_path, count_log_file = count_log_file)

	# Return validation results (TRUE for valid lines, FALSE for invalid)
	return(validation_results)
}
