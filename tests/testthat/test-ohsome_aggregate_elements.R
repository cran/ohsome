q <- ohsome_elements_count("0,0,1000")

test_that("creates ohsome_query object with the correct endpoint", {
	expect_equal(httr::parse_url(q$url)$path, "v1/elements/count")
})

test_that("returns object of class ohsome_query", {
	expect_s3_class(q, "ohsome_query")
})

test_that("correctly sets ratio endpoint on return_value arg", {
	q <- ohsome_elements_length(return_value = "ratio")
	expect_equal(httr::parse_url(q$url)$path, "v1/elements/length/ratio")
})
