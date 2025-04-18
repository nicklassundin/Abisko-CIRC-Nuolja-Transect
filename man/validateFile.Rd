% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validateFile}
\alias{validateFile}
\title{Validate All Lines in a File Against a Specific Format}
\usage{
validateFile(file_path, silent = FALSE)
}
\arguments{
\item{file_path}{A character vector representing the path to the file to be validated.}
}
\value{
A logical vector where each element indicates whether the corresponding line
in the file matches the specified format pattern.
}
\description{
This function reads a file line by line and validates each line against a specified
format pattern. It prints and optionally logs error messages for lines that do
not match the expected format.
}
\examples{
validateFile("data.txt")

}
