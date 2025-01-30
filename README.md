# Nuolja Transect

## Project Overview
The Nuolja Transect project involves processing data from transect surveys conducted in the Nuolja region. The key components of the project include:

### Directory structure
<!-- TREE START -->
#### Directory Tree
├── build.docs.R
├── data
│   └── README.md
├── descriptions
├── docs
│   ├── reference
├── log
├── man
├── plots
│   ├── README.md
├── R
│   ├── generate.R
│   ├── helper.R
│   ├── repack.R
│   └── validate.R
├── README.md
├── script.R
<!-- TREE END -->

### Data Processing
- **Path Functions**: Functions to retrieve paths and directories within the data directory.
- **Filtering Functions**: Functions to filter data based on historical and contemporary perspectives.
- **Date Handling Functions**: Functions to extract and format dates from filenames.
- **Data Reading Functions**: Functions to read and process data files, inserting entries into target data structures.

### Key Functions and Their Purpose
- **getPaths()**: Retrieves the paths of directories within a specified directory, optionally matching a pattern.
- **getDirs()**: Retrieves the names of directories within a specified directory, optionally matching a pattern.
- **getDataFilesPaths()**: Retrieves the paths of data files within a specified directory, optionally matching a pattern.
- **historical()**: Filters input to identify historical perspective.
- **contemporary()**: Filters input to identify contemporary perspective.
- **noneNum()**: Filters out non-numerical characters from input.
- **createDateAndFix()**: Converts a string to a Date object, fixing format if necessary.
- **formatDate()**: Extracts and formats dates from specific string formats.
- **extract_date()**: Extracts the date from a filename in YYYYMMDD format.
- **readFile()**: Reads and processes a data file, returning a structured data frame.
- **insert()**: Inserts a new entry into a target data structure.
- **drawPlots()**: Generates and saves plots based on the provided data frame.
- **dataframeBuilder()**: Builds a data frame by accumulating rows from provided data.

### Example Data
An example of the data format used in this project can be found in the `descriptions/transect_description.csv` file. This file contains information about the transect points, including plot numbers, coordinates (latitude and longitude), and elevations.

## Instructions
1. Place your data directories into the `data` directory.
2. Run `Rscript script.r` to process the data. This will generate files in the `/repack` directory and output files in the `/out` directory.
3. Follow the prompted options to validate or generate files as required.

## Notes about Data
- There are inconsistencies in date naming within the dataset.
- One entry from 2022 is missing a valid date.

## Documentation
The full documentation for this project is available as a GitHub Pages site. You can access it [here](https://nicklassundin.github.io/Abisko-CIRC-Nuolja-Transect/).

This documentation includes detailed information about the project's structure, data processing steps, and usage examples. It is generated automatically from the source code comments using `roxygen2` and `pkgdown`.

### How to Update Documentation
1. Ensure your code is properly documented using `roxygen2` comments.
2. Push your changes to the `beta` branch.
3. The GitHub Actions workflow will automatically generate and deploy the updated documentation to GitHub Pages.

### Example
To see an example of how to document your functions, check the comments in `script.r`:

```r
#' Title of your function
#'
#' Detailed description of your function.
#'
#' @param x Description of parameter x.
#' @param y Description of parameter y.
#' @return Description of the return value.
#' @export
#' @examples
#' example_function(1, 2)
example_function <- function(x, y) {
# Your code here


}
```
