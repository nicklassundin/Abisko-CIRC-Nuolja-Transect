DATA_FILE_PATTERN = "^Nuolja Transect Phenology Data Entry Segments \\d+ to \\d+ \\d{4} CURRENT\\.xlsx$"

datasheet_info <- normalizePath('descriptions/Nuolja\ Master\ Documents/Nuolja_Phenology_Datasheet_Information.xlsx')
species_errors <- normalizePath('descriptions/Nuolja\ Master\ Documents/Nuolja\ Species\ Errors\ and\ Observation\ Notes\ CURRENT.xlsx')

datasheet_info <- read.xlsx(datasheet_info, sheet = 1, colNames = TRUE)



survey_data_sheet_get <- function(species_list, poles, i){

	poles_species <- species_list %>%
		filter(`Poles` == poles[i] | `Poles` == poles[i+1]) %>%
		distinct(`Synonym Current`, Poles)
	poles_species <- poles_species %>%
		group_by(`Synonym Current`) %>%
		mutate(PoleNum = case_when(`Poles` == poles[i] ~ "1",
					   `Poles` == poles[i + 1] ~ "2")) %>%
		left_join(datasheet_info, by = c("Synonym Current" = "Species")) %>%
		group_by(`Synonym Current`) %>%
		summarise(
			  PoleNums = sort(unique(PoleNum)),
			  num_poles = n_distinct(PoleNum),
			  W = any(W == "Y", na.rm = TRUE),
			  WG = any(WG == "Y", na.rm = TRUE),
			  .groups = "drop"
			  ) %>%
		mutate(tag = case_when(
				       num_poles == 2 & (W == TRUE) & (WG == TRUE) ~ "(W, WG)",
				       num_poles == 2 & (W == TRUE) & (WG == FALSE) ~ "(W)",
				       num_poles == 2 & (W == FALSE) & (WG == TRUE) ~ "(WG)",
				       num_poles == 2 ~ "",
				       W & WG ~ paste0("(", PoleNums[1], ", W, WG)"),
				       W ~ paste0("(", PoleNums[1], ", W)"),
				       WG ~ paste0("(", PoleNums[1], ", WG)"),
				       num_poles == 1 ~ paste0("(", PoleNums[1], ")")
				       ),`Synonym Current` = if_else(tag != "",
				       paste0(`Synonym Current`, " ", tag),
				       `Synonym Current`));
		poles_species <- poles_species %>%
			distinct(`Synonym Current`);
	return(poles_species)
}

#' @title survey_tables
#' @description This function creates survey tables for the given data frame
#' @param df The data frame to create survey tables for
#' @return A data frame containing the survey tables
#' @export
build_species_list <- function(df){

	species_counts <- df %>%
		distinct(`Synonym Current`, Poles, Year) %>%
		count(`Synonym Current`, Poles);
	species_list <- df %>% 
		group_by(`Synonym Current`, Poles, Year) %>% 
		summarise(n = n(), .groups = "drop") %>%
		select(-n)
	species_list <- species_list %>%
		left_join(species_counts, by = c("Synonym Current", "Poles")) %>%
		filter(n > 1)


	# Observation Error
	species_errors <- read.xlsx(species_errors, sheet = 1, colNames = TRUE)
	species_corrected_list <- species_errors$`Observed.species` %>% unique()
	# remove columns
	species_errors <- species_errors[,c(1,2,3,4,5,7,8,9,10)]
	# print(species_errors[1:10,])
	# filter based on species errors 'Species Error (Y/N)' is Y

	species_list <- species_list %>%
		left_join(species_errors, 
			  by = c("Synonym Current" = "Observed.species", "Year" = "Year", "Poles" = "Subplot")) 
	# by = c("Synonym Current" = "Observed.species", "Year" = "Year", "Poles" = "Subplot")) 

	# print(species_list[,2])
	mask = !is.na(species_list$`Corrected.name`) & species_list$`Species.Error.(Y/N)` == "Y"; 
	species_list[mask,]$`Synonym Current` = species_list[mask,]$`Corrected.name`
	species_list <- species_list %>%
		mutate(`Field Filter` = 
		       (!is.na(`Corrected.name`) & (`Species.Error.(Y/N)` == "Y")) | (`Species.Error.(Y/N)` == "N") &
		       (`single.date.observation.(Y/N)` == "N") &
		       (`High.confidence.of.correct.identification.on.species.level.(Y/N)` == "Y") &
			(`primtive.plant.(Y/N)` == 'N'))

	species_list <- species_list %>%
		filter(`Field Filter` == TRUE)

	species_list <- species_list[order(species_list$Poles),]
	# list of poles
	poles <- species_list %>%
		distinct(Poles, .keep_all = TRUE) %>%
		select(Poles);
	# poles to array
	poles <- as.character(poles$Poles)

	# print(poles[1:10])
	# Boolean table for Poles
	species_list <- species_list[order(species_list$`Synonym Current`),]
	# Exclude if not in corrected species list
	species_list <- species_list %>%
		filter(`Synonym Current` %in% species_corrected_list)

	# select(-W) %>% select(-WG)
	# print(species_list[,c(1,12:20)])

	# print(colnames(species_list)[11:20])
	# return(TRUE)
	return(list(species_list = species_list, poles = poles))
}

build_data_sheets <- function(species_list, poles, file_name = "out/Planet Phenology Survey/Nuolja Transect Phenology Datasheets.xlsx"){
	wb <- createWorkbook()
	dir.create("out/Planet Phenology Survey", showWarnings = FALSE, recursive = TRUE)

	top_header <- matrix(c("Date:", "Surveyors:", ""), nrow = 1)
	phen_sub_head <- c("Confirmed ID", "Leaf-out", "Flowering", "Fruiting", "Seed Dispersal", "Senescence", "Leaf Fall")
	phen_header <- matrix(c("Phenology Phases", phen_sub_head, phen_sub_head), nrow = 1)
	# iterate over the poles by pair neigboors
	for (i in seq(1, length(poles), 2)){
		sheet = paste0(substr(poles[i],1,2), "-", substr(poles[i+1],7,8))
		addWorksheet(wb, sheet);
		# Create a centering style
		centerStyle <- createStyle(halign = "center", valign = "center")
		# Apply centering to the whole used range
		addStyle(wb, sheet = sheet, style = centerStyle,
			 rows = 2:3, cols = 2:15, gridExpand = TRUE)

		setColWidths(wb, sheet = sheet, cols = 1, widths = 35)
		writeData(wb, sheet, x = top_header, startCol = 1, startRow = 1, colNames = FALSE)
		mergeCells(wb, sheet, cols = 2:15, rows = 1)
		poles_header <- matrix(c("Subplot", poles[i],"","","","","","", poles[i+1]), nrow = 1)
		writeData(wb, sheet, x = poles_header, startCol = 1, startRow = 2, colNames = FALSE)
		mergeCells(wb, sheet, cols = 2:8, rows = 2)
		mergeCells(wb, sheet, cols = 9:15, rows = 2)
		writeData(wb, sheet, x = phen_header, startCol = 1, startRow = 3, colNames = FALSE)
		# Create vertical text style (textRotation = 90 for vertical)
		verticalStyle <- createStyle(textRotation = 90, halign="center", textDecoration = "bold", 
					     valign = "center", wrapText = TRUE, fontSize = 10)
		# Apply style to header row (row 2)
		setRowHeights(wb, sheet = sheet, rows = 3, heights = 75)
		addStyle(wb, sheet = sheet, style = verticalStyle, 
			 rows = 3, cols = 2:15, gridExpand = TRUE)
		# Bold for top rows
		boldStyle <- createStyle(textDecoration = "bold", valign = "center")
		addStyle(wb, sheet = sheet, style = boldStyle,
			 rows = 2, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		addStyle(wb, sheet = sheet, style = boldStyle,
			 rows = 3, cols = 1, gridExpand = TRUE, stack = TRUE)
		# Bold for top header
		addStyle(wb, sheet = sheet, style = boldStyle,
			 rows = 1, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		setRowHeights(wb, sheet = sheet, rows = 1, heights = 25)
		poles_species <- survey_data_sheet_get(species_list, poles, i)
		writeData(wb, sheet, x = poles_species$`Synonym Current`, startCol = 1, startRow = 4, colNames = FALSE)

		# Create fill style
		fillStyle <- createStyle(
					 fgFill = "#D9D9D9",
		)

		# Get total number of rows (including header)
		total_rows <- 100+3 # +1 for header

		# Apply style in blocks: color 3 rows, skip 3
		for (start_row in seq(4, total_rows, by = 6)) {
			rows_to_color <- start_row:min(start_row + 2, total_rows)
			addStyle(wb, sheet = sheet, style = fillStyle,
				 rows = rows_to_color, cols = 1:15, gridExpand = TRUE)

		}
		gridStyle <- createStyle(border = "TopBottomLeftRight", borderStyle = "thin")
		addStyle(wb, sheet = sheet, style = gridStyle,
			 rows = 2:100, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		gridStyleThick <- createStyle(border = "TopBottomLeftRight", borderStyle = "medium")
		addStyle(wb, sheet = sheet, style = gridStyleThick,
			 rows = 1:3, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		addStyle(wb, sheet = sheet, style = gridStyleThick,
			 rows = 1:100, cols = 1, gridExpand = TRUE, stack = TRUE)

	}
	saveWorkbook(wb, file_name, overwrite = TRUE)
}

spring_survey_names <- function(species_list, poles, i){

	poles_species <- species_list %>%
		filter(`Poles` == poles[i] | `Poles` == poles[i+1]) %>%
		distinct(`Synonym Current`, Poles)
	poles_species <- poles_species %>%
		group_by(`Synonym Current`) %>%
		mutate(PoleNum = case_when(`Poles` == poles[i] ~ "1",
					   `Poles` == poles[i + 1] ~ "2")) %>%
		left_join(datasheet_info, by = c("Synonym Current" = "Species")) %>%
		group_by(`Synonym Current`) %>%
		summarise(
			  PoleNums = sort(unique(PoleNum)),
			  num_poles = n_distinct(PoleNum),
			  W = any(W == "Y", na.rm = TRUE),
			  WG = any(WG == "Y", na.rm = TRUE),
			  .groups = "drop"
			  ) %>%
		mutate(tag = case_when(
				       num_poles == 2 & (W == TRUE) & (WG == TRUE) ~ "(W, WG)",
				       num_poles == 2 & (W == TRUE) & (WG == FALSE) ~ "(W)",
				       num_poles == 2 & (W == FALSE) & (WG == TRUE) ~ "(WG)",
				       num_poles == 2 ~ "",
				       W & WG ~ paste0("(", PoleNums[1], ", W, WG)"),
				       W ~ paste0("(", PoleNums[1], ", W)"),
				       WG ~ paste0("(", PoleNums[1], ", WG)"),
				       num_poles == 1 ~ paste0("(", PoleNums[1], ")")
				       ),`Synonym Current` = if_else(tag != "",
				       paste0(`Synonym Current`, " ", tag),
				       `Synonym Current`));
		poles_species <- poles_species %>%
			distinct(`Synonym Current`);
	return(poles_species)
}

build_spring_data_sheets <- function(species_list, poles, file_name = "out/Planet Phenology Survey/Nuolja Transect Phenology Datasheets.xlsx"){
	wb <- createWorkbook()
	dir.create("out/Planet Phenology Survey", showWarnings = FALSE, recursive = TRUE)

	top_header <- matrix(c("Date:", "Surveyors:", ""), nrow = 1)
	phen_sub_head <- c("Confirmed ID", "Leaf-out", "Flowering", "Fruiting", "Seed Dispersal", "Senescence", "Leaf Fall")
	phen_header <- matrix(c("Phenology Phases", phen_sub_head, phen_sub_head), nrow = 1)
	# iterate over the poles by pair neigboors
	for (i in seq(1, length(poles), 14)){
		# sheet = paste0(substr(poles[i],1,2), "-", substr(poles[i+1],7,8))
		sheet = paste0(substr(poles[i],1,2), "-", substr(poles[i+1],7,8))
		addWorksheet(wb, sheet);
		# Create a centering style
		centerStyle <- createStyle(halign = "center", valign = "center")
		# Apply centering to the whole used range
		addStyle(wb, sheet = sheet, style = centerStyle,
			 rows = 2:3, cols = 2:15, gridExpand = TRUE)

		setColWidths(wb, sheet = sheet, cols = 1, widths = 35)
		writeData(wb, sheet, x = top_header, startCol = 1, startRow = 1, colNames = FALSE)
		mergeCells(wb, sheet, cols = 2:15, rows = 1)
		poles_header <- matrix(c("Subplot", poles[i],"","","","","","", poles[i+1]), nrow = 1)
		writeData(wb, sheet, x = poles_header, startCol = 1, startRow = 2, colNames = FALSE)
		mergeCells(wb, sheet, cols = 2:8, rows = 2)
		mergeCells(wb, sheet, cols = 9:15, rows = 2)
		writeData(wb, sheet, x = phen_header, startCol = 1, startRow = 3, colNames = FALSE)
		# Create vertical text style (textRotation = 90 for vertical)
		verticalStyle <- createStyle(textRotation = 90, halign="center", textDecoration = "bold", 
					     valign = "center", wrapText = TRUE, fontSize = 10)
		# Apply style to header row (row 2)
		setRowHeights(wb, sheet = sheet, rows = 3, heights = 75)
		addStyle(wb, sheet = sheet, style = verticalStyle, 
			 rows = 3, cols = 2:15, gridExpand = TRUE)
		# Bold for top rows
		boldStyle <- createStyle(textDecoration = "bold", valign = "center")
		addStyle(wb, sheet = sheet, style = boldStyle,
			 rows = 2, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		addStyle(wb, sheet = sheet, style = boldStyle,
			 rows = 3, cols = 1, gridExpand = TRUE, stack = TRUE)
		# Bold for top header
		addStyle(wb, sheet = sheet, style = boldStyle,
			 rows = 1, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		setRowHeights(wb, sheet = sheet, rows = 1, heights = 25)
		poles_species <- spring_survey_names(species_list, poles, i)
		writeData(wb, sheet, x = poles_species$`Synonym Current`, startCol = 1, startRow = 4, colNames = FALSE)

		# Create fill style
		fillStyle <- createStyle(
					 fgFill = "#D9D9D9",
		)

		# Get total number of rows (including header)
		total_rows <- 100+3 # +1 for header

		# Apply style in blocks: color 3 rows, skip 3
		for (start_row in seq(4, total_rows, by = 6)) {
			rows_to_color <- start_row:min(start_row + 2, total_rows)
			addStyle(wb, sheet = sheet, style = fillStyle,
				 rows = rows_to_color, cols = 1:15, gridExpand = TRUE)

		}
		gridStyle <- createStyle(border = "TopBottomLeftRight", borderStyle = "thin")
		addStyle(wb, sheet = sheet, style = gridStyle,
			 rows = 2:100, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		gridStyleThick <- createStyle(border = "TopBottomLeftRight", borderStyle = "medium")
		addStyle(wb, sheet = sheet, style = gridStyleThick,
			 rows = 1:3, cols = 1:15, gridExpand = TRUE, stack = TRUE)
		addStyle(wb, sheet = sheet, style = gridStyleThick,
			 rows = 1:100, cols = 1, gridExpand = TRUE, stack = TRUE)

	}
	saveWorkbook(wb, file_name, overwrite = TRUE)
}


