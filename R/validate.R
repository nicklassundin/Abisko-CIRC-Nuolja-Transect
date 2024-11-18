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
	pattern <- "^NS-\\d{8}-\\d{3}[ , ]\\d+\\.\\d+N[ , ]\\d+\\.\\d+E[, ]\\d+\\.\\d+[, ][osOS]{1,2}$"

	# Check if the line matches the pattern
	validation_result <- grepl(pattern, line)

	# If line is invalid and a log_file is provided, log errors
	if (!validation_result && !is.null(log_file)) {
		printValidationError(line, file, line_number, log_file)

	}

	return(validation_result)

}

#' Print Validation Error Messages for Invalid Lines
#'
#' This function prints detailed error messages for lines that do not match the expected 
#' format pattern. Errors can be logged to a specified log file for later review.
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
#' printValidationError("NS-20220510-001 68.37261219N 18.69783 1195.186 X", "data.txt", 5, "log/validation.txt")
#' 
#' @export
printValidationError <- function(line, file = NA, line_number = NA, log_file = NULL, error_list = NULL) {
		error_message <- NULL

if (!grepl("^NS-", line)) {
			error_message <- "Error: Line does not start with 'NS-' prefix.\n"
	
} else if (!grepl("\\d{8}-\\d{3}", line)) {
			error_message <- "Error: Missing or incorrect date-time format (YYYYMMDD-NNN).\n"
	
} else if (!grepl("\\d+\\.\\d{6,9}[N]", line)) {
			error_message <- "Error: Missing or incorrect latitude format.\n"
	
} else if (!grepl("\\d+\\.\\d{6,9}[E]", line)) {
			error_message <- "Error: Missing or incorrect longitude format.\n"
	
} else if (!grepl("\\d+\\.\\d+ [osOS]{1,2}$", line)) {
			error_message <- "Error: Missing or incorrect observation code format (ends with 'O' or 'os').\n"
	
}

	# Log error messages
if (!is.null(error_message)) {
			# Append the error to the list if provided
	if (!is.null(error_list)) {
					error_list[[length(error_list) + 1]] <- error_message
			
	}

			# Print the error message
			cat(error_message)
			cat("File: ", file, "\n")
					cat("Line number: ", line_number, "\n")
					cat("Line: ", line, "\n")

							# Log the error message to the file if provided
					if (!is.null(log_file)) {
						log_entry <- paste0(
								    				error_message, 
																"File: ", file, "\n",
																"Line number: ", line_number, "\n",
																				"Line: ", line, "\n\n"
																			
						)
									cat(log_entry, file = log_file, append = TRUE)
								
					}
						
}
	return(error_list)
	
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
validateFile <- function(file_path) {
	log_file <- "log/validation.txt"
	# Read the file line by line
	lines <- readLines(file_path)
	validation_results <- logical(length(lines))

	# Validate each line
	for (i in seq_along(lines)) {
		validation_results[i] <- validateLine(lines[i], file = file_path, line_number = i, log_file = log_file)

	}

	# Return validation results (TRUE for valid lines, FALSE for invalid)
	return(validation_results)

}
