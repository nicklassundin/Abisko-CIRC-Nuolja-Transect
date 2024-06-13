context("pathing")
test_that("getPath", {
		  paths <- getPaths("/data")
		  expect_that(paths[1], equals(paste(getwd(), "/data/2018", sep="")))

})
test_that("getDirs", {
		  dirs <- getDirs("/data")
		  expect_that(dirs[1], equals("2018"))

})
test_that("getDataFilesPaths", {
		  path <- getPaths("/data")
		  paths <- getDataFilesPaths(path)
		  expect_that(paths[1], equals(paste(getwd(), "/data/2018/20180510.csv", sep="")))

})

## Historical and contemporary filter functions
context("filter")
context("historical")
test_that("historical", {
		  l <- list("so", "s", "os", "r", "o")
		  answer <- list("s", "s", "s", "o", "o")
		  result <- lapply(l, historical)
		  expect_that(result, equals(answer))

})
context("contemporary")
test_that("contemporary", {
		  l <- list("so", "s", "os", "r", "o")
		  answer <- list("so", "s", "os", "o", "o")
		  result <- lapply(l, contemporary)
		  expect_that(result, equals(answer))

})

context("none Numerical")
test_that("noneNum", {
		  l <- c("10N", "20N", "30N", "40N", "50N")
		  answer <- c("10", "20", "30", "40", "50")
		  result <- delist(lapply(l, noneNum))
		  expect_that(result, equals(answer))

})

dates <- c("S-20200724", "S-20200724-001Q", "S-20200724-002", "NS-20200724-003", "NS20200724-004", "NS200724-004")
answer <- "20200724"
context("format dates")
test_that(paste("case 1:", dates[1]), {
		  result <- formatDate(dates[1])
		  expect_that(result, equals(as.Date(answer, "%Y%m%d")))

})
test_that(paste("case 2:", dates[2]), {
		  result <- formatDate(dates[2])
		  expect_that(result, equals(as.Date(answer, "%Y%m%d")))

})
test_that(paste("case 3:", dates[3]), {
		  result <- formatDate(dates[3])
		  expect_that(result, equals(as.Date(answer, "%Y%m%d")))

})
test_that(paste("case 4:", dates[4]), {
		  result <- formatDate(dates[4])
		  expect_that(result, equals(as.Date(answer, "%Y%m%d")))

})
test_that(paste("case 5:", dates[5]), {
		  result <- formatDate(dates[5])
		  expect_that(result, equals(as.Date(answer, "%Y%m%d")))

})
test_that(paste("case 6:", dates[6]), {
		  result <- formatDate(dates[6])
		  expect_that(result, equals(as.Date(answer, "%Y%m%d")))

})

options(warn = -1)
test_that("extract_date function extracts date from filename", {
		  # Test filename with date in YYYYMMDD format
		  filename1 <- "somefile_20220510.csv"
		  expected_date1 <- "20220510"
		  result1 <- extract_date(filename1)
		  expect_true(result1 == expected_date1)

		  # Test filename without date
		  filename2 <- "anotherfile.csv"
		  expect_true(is.null(extract_date(filename2)))

		  # Test filename with date but not in the correct format
		  filename3 <- "file_2023_05_18.csv"
		  expect_true(is.null(extract_date(filename3)))

})
options(warn = 0)

context("data processing")
test_that("insert function works correctly", {
		  target <- data.frame(matrix(ncol = 3, nrow = 0))
		  entry <- data.frame(matrix(1:3, ncol = 3))
		  sect <- c("a", "b", "c")
		  result <- insert(target, entry, sect)
		  expect_equal(nrow(result), 1)
		  expect_equal(result[1, 1], "a")

})

test_that("delist function works correctly", {
		  l <- list(c("1", "2"), c("3", "4"))
		  result <- delist(l)
		  expect_equal(result, c("1, 2", "3, 4"))

})

test_that("getName function works correctly", {
		  sourceFileName <- "sourceFile"
		  specification <- "spec"
		  result <- getName(sourceFileName, specification)
		  expect_equal(result, "out/sourceFile_spec.csv")

})
