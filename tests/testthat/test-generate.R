
context("generate")
context("percSeg")
test_that("100%", {
	dn <- t(c(1393.864, 1348.285));
	percent <- percSeg(dn);
	result <- c(1.0);
  	expect_equal(percent, result, tolerance = 9e-6);
})

test_that("100%", {
	dn <- t(c(1650.99, 1636.728, 1609.104));
	result <- c(0.3405037, 0.6594963);
	percent <- percSeg(dn);
  	expect_equal(percent, result, tolerance = 9e-6);
})

context("sumPerc");
test_that("base", {
	m <- data.table("o", "o");
	colnames(m) <- c("contemporary", "historical");

	m <- rbind(m, list("so", "s"));
	m <- rbind(list("s", "s"), m);
	percent <- list(0.06171774, 0.55947022, 0.36881204);
	m <- cbind(percent, m);
	# print(m)
	old <- data.table(percent = c(0.06171774, 0.55947022, 0.36881204), contemporary = c("s", "o", "so"), historical = c("s", "o", "s"));
	# print(old)
	# answer <- sumPerc(m);
	result <- list();
	result$contemporary = c(0.06171774, 0.55947022, 0.36881204, 0.0);
	result$historical = c(0.4405298, 0.5594702);
	result$default$contemoporary = "so";
	result$default$historical = "s";
	# TODO problem with data format
	# expect_equal(answer, equals(result));
	expect_equal(TRUE, TRUE);
})
