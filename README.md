# Nuolja Transect

## Project Overview
<details>
The Nuolja Transect project involves processing data from transect surveys conducted in the Nuolja region. The key components of the project include:

### Directory structure
<details>
<!-- TREE START -->
#### Directory Tree
```tree
project_directory/
├── build.docs.R
├── data
│   ├── DATAHERE.md
├── descriptions
├── docs
│   ├── reference
├── man
├── old.script.R
├── out
├── plots
│   ├── README.md
├── R
│   ├── generate.R
│   ├── helper.R
│   ├── patterns.R
│   ├── repack.R
│   └── validate.R
├── README.md
├── repack
├── script.R
```
<!-- TREE END -->
</details>

### Data Processing
- **Path Functions**: Functions to retrieve paths and directories within the data directory.
- **Filtering Functions**: Functions to filter data based on historical and contemporary perspectives.
- **Date Handling Functions**: Functions to extract and format dates from filenames.
- **Data Reading Functions**: Functions to read and process data files, inserting entries into target data structures.

### Key Functions and Their Purpose
#### Validation 
Validate the data in and log the errors into directory log/
### Example Data
An example of the data format used in this project can be found in the `descriptions/transect_description.csv` file. This file contains information about the transect points, including plot numbers, coordinates (latitude and longitude), and elevations.
#### Notes about Data
- There are inconsistencies in date naming within the dataset.
- One entry from 2022 is missing a valid date.
- Geotaging are inconsistent throught the dataset
</details>

## Operations
<details>

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

</details>

## Prerequisites
<details>
To run the project, ensure the following are installed and configured on your system:
1. **R and Rtools**  
   - Install the latest version of R from [CRAN](https://cran.r-project.org/).  
   - If on Windows, install Rtools for building and compiling packages.

2. **Required R Packages**  
   These will be install when running the script
   - `dplyr`
   - `ggplot2`
   - `tidyr`
   - `lubridate`
   - `readr`
   - `devtools`
   - `roxygen2`
   - `pkgdown`
</details>

## Instructions
<details>
   <summary>Running the Code in interactive enviroment</summary>
   1. Place your data directories into the `data` directory.
2. Run `Rscript script.r` to process the data. This will generate files in the `/repack` directory and output files in the `/out` directory.
3. Follow the prompted options to validate or generate files as required.

### Running an Interactive R Session in RStudio Terminal
Follow these steps to run your R script interactively in RStudio's terminal, ensuring the working directory is set correctly:

## Running an Interactive R Session in RStudio Terminal

Follow these steps to run your R script interactively in RStudio's terminal, ensuring the working directory is set correctly:

### Step 1: Change the Working Directory in RStudio
Before opening the terminal, set the working directory in RStudio using one of these methods:

#### Option 1: Use the GUI
1. Click on `Session > Set Working Directory > Choose Directory...`.
2. Navigate to the folder containing your R script and click `Open`.

#### Option 2: Use the Console
1. In the RStudio console, set the working directory manually by typing:
   ```r
   setwd("path/to/your/script")
### Step 2: Open the Terminal in RStudio
- In RStudio, go to `Tools > Terminal > New Terminal`.  
  Alternatively, use the shortcut:  
  - **Windows/Linux**: `Shift + Alt + T`  
  - **Mac**: `Shift + Option + T`

### 
- type
     ```r
   Rscript script.R
- This will run the script
```
</details>

## Documentation
<details>
The full documentation for this project is available as a GitHub Pages site. You can access it [here](https://nicklassundin.github.io/Abisko-CIRC-Nuolja-Transect/).

This documentation includes detailed information about the project's structure, data processing steps, and usage examples. It is generated automatically from the source code comments using `roxygen2` and `pkgdown`.

### How to Update Documentation
#### Manually
1. Enter `R` enviroment
2. Run `devtools::document()`
3. Run `pkgdown::build_site()`
4. Exit `R` enviroment
3. push to repository
4. pull into master

<!-- #### Github workflow -->
<!-- 1. Ensure your code is properly documented using `roxygen2` comments. -->
<!-- 2. Push your changes to the `beta` branch. -->
<!-- 3. The GitHub Actions workflow will automatically generate and deploy the updated documentation to GitHub Pages. -->
</details>

## Data Formating
<details>

### File Format Specification for `repack/`

The files in the `repack/` directory are structured as CSV files with detailed information about geographical plots and their associated data. Each file adheres to the following schema:

#### Snow Data
<details>
##### Column Descriptions

| **Column Name**  | **Description**                                                                                              |
|-------------------|------------------------------------------------------------------------------------------------------------|
| `plot`           | The plot number associated with the data entry.                                                            |
| `subplot`        | The subplot number within the plot.                                                                         |
| `proj_factor`    | A calculated projection factor, used for scaling or alignment in analyses.                                  |
| `id`             | A unique identifier for each record, formatted as `NS-YYYYMMDD-XXX`, where `XXX` is the sequential entry.  |
| `date`           | The date of the record, formatted as `YYYY-MM-DD`.                                                         |
| `latitude`       | The latitude of the recorded point in decimal degrees.                                                     |
| `longitude`      | The longitude of the recorded point in decimal degrees.                                                    |
| `elevation`      | The elevation at the specific point, measured in meters.                                                   |
| `contemporary`   | A label indicating the contemporary observation status. Possible values:                                    |
|                  | - `o`: Open                                                                                         |
|                  | - `s`: Snow                                                                                              |
|                  | - `os`: Both Open and Snow                                                                        |
| `historical`     | A label indicating the historical observation status. Possible values:                                     |
|                  | - `o`: Open                                                                                         |
|                  | - `s`: Snow                                                                                              |

##### File Characteristics

- **CSV Format**: The files are plain-text, comma-separated value files with a header row for column names.
- **Consistency**: Each row corresponds to a single data point, and all columns are present for every entry.
- **Data Use**: These files are used for analyzing environmental or geographical changes across plots and subplots.

##### Example Data

Below is an excerpt to illustrate the format:

```csv
"plot","subplot","proj_factor","id","date","latitude","longitude","elevation","contemporary","historical"
20,78,3357.62764497642,"NS-20180506-001","2018-05-06","68.37261122","18.69783956",1180.841,"o","o"
19,76,3260.95020778743,"NS-20180506-004","2018-05-06","68.37218199","18.69989872",1169.419,"os","s"
18,69,2957.15889307984,"NS-20180506-011","2018-05-06","68.37041561","18.70585272",1103.361,"s","s"
```
</details>
### Phenology Data
TODO
<details>
</details>
### File Format Specification for `out/`

The files in the `out/` directory include CSV files with data representing daily snow of various plot statuses. Each file adheres to the following schema:

#### Snow Data
<details>
##### Column Descriptions

| **Column Name** | **Description**                                                                                   |
|------------------|---------------------------------------------------------------------------------------------------|
| `DOY`           | Day of the Year (DOY) for the recorded observations.                                              |
| `plot/subplot`  | The plot number associated with the data entry.                                                   |
| `s`             | Proportion of open categorized as "Snow" for the given plot and day.                    |
| `so`            | Proportion of open categorized as "Snow and Open" for the given plot and day.    |
| `o`             | Proportion of open categorized as "Open" for the given plot and day.               |
| `os`            | Proportion of open categorized as "Open and Snow" for the given plot and day.    |

##### File Characteristics

- **CSV Format**: The files are plain-text, comma-separated value files with a header row for column names.
- **Proportional Data**: The columns `s`, `so`, `o`, and `os` represent proportions (values between 0 and 1) for each category.
- **Daily Observations**: Each row corresponds to a specific day and plot.

##### Example Data

Below is an excerpt to illustrate the format:

```csv
"DOY","plot","s","so","o","os"
126,6,0.0540559471516565,0.21497303215613,0.0818149541820005,0.649156066510213
126,7,0,0.318104640512793,0.139848133706484,0.542047225780723
126,8,0.123763616329758,0.876236383670242,0,0
126,9,0,0.863436913617926,0,0.136563086382074
126,10,0.768916411918146,0,0.0997298448068066,0.13135374327
```
</details>
#### Phenology Data
TODO
<details>
</details>

</details>
