
context("validate")
context("validate line format")


# crate dataframe with NS-20180510-001 68.37261219N 18.69783E 1195.186 O

context("validate ID format")
test_that("NS-20180510-001", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 1195.186 O", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})
test_that("NS230830", {
	df <- data.frame(line = "NS230830 68.37261219N 18.69783E 1195.186 O", stringsAsFactors = FALSE)
	expect_false(validateLine(df$line))
})

context("longitude")
test_that("68.366449N", {
	df <- data.frame(line = "NS-20180510-001 68.366449N 18.69783E 1195.186 O", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})

context("latitude")
test_that("18.719657E", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.719657E 1195.186 O", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})
context("transect")
test_that("1195.186", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 1195.186 O", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})

test_that("928.708", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 928.708 O", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})


context("validate observation code")
test_that("O", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 1195.186 O", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})
test_that("o", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 1195.186 o", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})
test_that("S", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 1195.186 S", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})
test_that("s", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 1195.186 s", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})
test_that("so", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 1195.186 so", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})
test_that("SO", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 1195.186 SO", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})
test_that("os", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 1195.186 os", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})
test_that("OS", {
	df <- data.frame(line = "NS-20180510-001 68.37261219N 18.69783E 1195.186 OS", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})

context("combined validation")
test_that("NS-20180714-004 68.366449N 18.719767E 928.708 o", {
	df <- data.frame(line = "NS-20180714-004 68.366449N 18.719767E 928.708 o", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})
test_that("NS-20180714-004,68.366449N,18.719767E,928.708,o", {
	df <- data.frame(line = "NS-20180714-004,68.366449N,18.719767E,928.708,o", stringsAsFactors = FALSE)
	expect_true(validateLine(df$line))
})

# Define the test case
test_that("validateFile function works correctly", {
		  # Create a temporary file with sample valid and invalid lines
		  temp_file <- tempfile()
		  valid_line <- "NS-20180510-001 68.37261219N 18.69783E 1195.186 O"
		  invalid_line <- "Invalid line"
		  writeLines(c(valid_line, invalid_line), temp_file)
		  # Test the validateFile function with the temporary file
		  result <- validateFile(temp_file);
		  expect_false(result)
		  # Clean up by deleting the temporary file
		  unlink(temp_file)
})
