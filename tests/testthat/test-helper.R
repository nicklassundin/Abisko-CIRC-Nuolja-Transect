
test_that("loadTransectDescription", {
		  # change path to point one directory up from current
		  setwd("../../")
		  path <- paste(getwd(), "/descriptions", sep="");


		  desc <- loadTransectDescription(path)

		  expect_equal(nrow(desc), 79)
})
test_that("getName", {
	expect_that(getName("test", "contemporary"), equals("out/test_contemporary.csv"));
})

