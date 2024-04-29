

context("pathing");
test_that("getPath", {
	paths <- getPaths("/data");
	expect_that(paths[1], equals(paste(getwd(), "/data/2018", sep="")));
})
test_that("getDirs", {
	dirs <- getDirs("/data");

	expect_that(dirs[1], equals("2018"));
})
test_that("getDataFilesPaths", {

	path <- getPaths("/data");
	paths <- getDataFilesPaths(path);
	expect_that(paths[1], equals(paste(getwd(), "/data/2018/20180510.csv", sep="")));
})

## Historical and contemporary filter functions
context("filter");
context("historical");
test_that("historical", {
	l <- list("so", "s", "os", "r", "o");
	answer <- list("s", "s", "s", "o", "o");
	result <- lapply(l, historical);
	expect_that(result, equals(answer));
})
context("contemporary");
test_that("contemporary", {
	l <- list("so", "s", "os", "r", "o");
	answer <- list("so", "s", "os", "o", "o");
	result <- lapply(l, contemporary);
	expect_that(result, equals(answer));
})

context("none Numerical");
test_that("noneNum", {
	l <- c("10N", "20N", "30N", "40N", "50N");
	answer <- c("10", "20", "30", "40", "50");
	result <- delist(lapply(l, noneNum));

	expect_that(result, equals(answer));
})

context("Readfile");
test_that("readfile", {
	path <- getPaths("/data");
	path <- getDataFilesPaths(path)[1];
	file <- readFile(path);
	file <- delist(file[1,2])
	expect_that(file, equals("2018-05-10"));
})
