context("sRm")

test_that("sRm-works", {
  example_file <- system.file("extdata/example_file.mzML", package = "sRm")

  srm_peaks <- openSRMfile(example_file)
  expect_true(isS4(srm_peaks))
  expect_that(nrow(srm_peaks@header), equals(length(srm_peaks@peaks)))
  expect_that(nrow(srm_peaks@header), equals(length(srm_peaks@index)))

  srm_comb <- combineTransitions(srm_peaks)
  expect_true(is.list(srm_comb))
  expect_that(length(srm_comb), equals(length(unique(srm_peaks@header$parentMz))))

})
