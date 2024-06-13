#' Validate a single line against a specific format.
#' 
#' This function checks whether a given line matches a specific format pattern.
#' 
#' @param line A character vector representing a single line to be validated.
#' 
#' @return A logical value indicating whether the line matches the format pattern.
#' 
#' @examples
#' validateLine("NS-20220510-001 68.37261219N 18.69783 1195.186 O")
#' 
#' @export

# Define the regular expression pattern
pattern <- "^NS-\\d{8}-\\d{3}[ ,]\\d+\\.\\d+N[ ,]\\d+\\.\\d+E[, ]\\d+\\.\\d+[, ][osOS]{1,2}$"
# pattern <- "^NS-\\d{8}-\\d{3} \\d+\\.\\d{6,9}[N] \\d+\\.\\d{6,9}[E] \\d+\\.\\d+ [osOS]{1,2}$"


validateLine <- function(line, file = NA, line_number = NA) {
	# Define the regular expression pattern
	# Check if the line matches the pattern
	printValidationError(line, file, line_number)
	return(grepl(pattern, line))
}
#' Print validation error messages for lines that do not match the format.
#' 
#' This function prints error messages for lines that do not match the expected format.
#' 
#' @param line A character vector representing a single line to be validated.
#' 
#' @return NULL
#' 
#' @examples
#' printValidationError("NS-20220510-001 68.37261219N 18.69783 1195.186 X")
#' 
#' @export
printValidationError <- function(line, file, line_number) {
	# Check if the line matches the pattern
	if (!grepl(pattern, line)) {
		# Print error message based on the missing information
		if (!grepl("^NS-", line)) {
			    cat("Error: Line does not start with 'NS-' prefix.\n")
		  
		} else if (!grepl("\\d{8}-\\d{3}", line)) {
			    cat("Error: Missing or incorrect date-time format (YYYYMMDD-NNN).\n")
		  
		} else if (!grepl("\\d+\\.\\d{6,9}[N]", line)) {
			    cat("Error: Missing or incorrect latitude format.\n")
		  
		} else if (!grepl("\\d+\\.\\d{6,9}[E]", line)) {
			    cat("Error: Missing or incorrect longitude format.\n")
		  
		} else if (!grepl("\\d+\\.\\d+ [osOS]{1,2}$", line)) {
			    cat("Error: Missing or incorrect observation code format (ends with 'O').\n")
		  
		}
		cat("File: ", file, "\n")
		cat("Line number: ", line_number, "\n")
		cat("Line: ", line, "\n")
			
	}
}

#' Validate each line in a file against a specific format.
#' 
#' This function reads a file and validates each line against a specific format pattern.
#' It prints error messages for lines that do not match the format.
#' 
#' @param file A character vector representing the path to the file to be validated.
#' 
#' @return A logical value indicating whether all lines in the file match the format pattern.
#' 
#' @examples
#' validateFile("data.txt")
#' 
#' @export'
validateFile <- function(file) {
	# Read the file
	lines <- readLines(file)
	# Check if each line is valid
	valid <- mapply(validateLine, lines, seq_along(lines), MoreArgs = list(file = file))
	# Return the result
	return(all(valid))
}
