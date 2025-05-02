################################################################################################
## Find errors in species names and species code                                              ##
################################################################################################
## Last modified by Johan Svedin on 19 Sep 2023                                               ##
################################################################################################


## To run this code, file must have been created using the script "Import and format 2023 data". ##


## required libraries for code ##
library(readxl);
library(lubridate);
library(data.table);
library(reshape2);
library(plyr);
library(dplyr);


## clear memory of all datasets ##
rm(list = ls())


## SET your working director ##
setwd("C:/Users/keith/Arctic Centre Dropbox/Arctic Ecologists Team Folder/Nuolja Data/Plant Phenology Data/Plant Phenology Raw Data 2023")


## Import Nuolja 2023 dataset from CSV ##
Nuolja.Data.2023 <- read.csv("Nuolja_Data_2023.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, stringsAsFactors=FALSE, na.strings=c("NA","NaN", " "))


## Import Phenology Codes dataset and reorder with only important columns ##
Pheno.Codes <- as.data.frame(read_excel("Plant List and Phenology Codes 2023.xlsx", sheet="Phenology Codes", col_names = TRUE, trim_ws=TRUE))


## Import plant list ##
Plant.List <- as.data.frame(read_excel("Plant List and Phenology Codes 2023.xlsx", sheet="Plant List", col_names = TRUE, trim_ws=TRUE, col_types = c("text"), range = cell_cols("A")))


## convert character to date for Date column ##
Nuolja.Data.2023$Date <- as.Date(Nuolja.Data.2023$Date, format="%Y-%m-%d")


## check for missing data in the Date column ##
na_error_rows <- which(rowSums(is.na(Nuolja.Data.2023[, c("Synonym.Current", "Date", "Subplot", "Code")])) > 0)


# Display the original rows with missing dates
original_error_rows <- Nuolja.Data.2023[na_error_rows, ]
Nuolja.Missing.Data <- data.frame(na_error_rows,original_error_rows)
colnames(Nuolja.Missing.Data) <- c("Row","Synonym Current","Date","Subplot","Code")
write.csv(Nuolja.Missing.Data, "Nuolja_Missing_Data.csv", row.names = FALSE)


## check for species names not in master list ##
Nuolja.Species.Errors <- setdiff(Nuolja.Data.2023$Synonym.Current,Plant.List$Scientific.Name)


## do not flag observations with genus only taxonomic determinations ##
#### Paul can you help with this? ####


write.csv(Nuolja.Species.Errors, "Nuolja_Species_Errors.csv", row.names=FALSE)


## check for phenology codes not in master list ##
Nuolja.Pheno.Code.Errors <- setdiff(Nuolja.Data.2023$Code,Pheno.Codes$Code)


## check for phenology code errors ## <-- Paul can you help
# leaf phenophase codes for non-tree and shrub species


write.csv(Nuolja.Pheno.Code.Errors, "Nuolja_Pheno_Code_Errors.csv", row.names=FALSE)


# Check for errors in a data frame and find specific rows
Nuolja.Pheno.Code.Errors <- setdiff(Nuolja.Data.2023$Code, Pheno.Codes$Code)
error_rows <- which(Nuolja.Data.2023$Code %in% Nuolja.Pheno.Code.Errors)
error_codes <- Nuolja.Data.2023$Code[error_rows]
Nuolja.Pheno.Code.Errors.Rows <- data.frame(error_rows,error_codes)


## Error checking to add ##


# 1. Check if there is data without species assigned to it.



