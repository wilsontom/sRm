context("sRm")

test_that("sRm-works", {
  example_file_local <- system.file("extdata/example_file.mzML", package = "sRm")

  srm_peaks <- openSRMfile(example_file_local)
  expect_true(isS4(srm_peaks))
  expect_that(nrow(srm_peaks@header), equals(length(srm_peaks@peaks)))
  expect_that(nrow(srm_peaks@header), equals(length(srm_peaks@index)))

  srm_comb <- combineTransitions(srm_peaks)
  expect_true(inherits(srm_comb, 'transition'))
  expect_that(length(srm_comb), equals(length(unique(srm_peaks@header$parent))))

  expect_true(is.null(transitions(srm_peaks)))
  expect_true(is.null(meta(srm_peaks)))
  expect_true(isS4(new("SRM")))

  })
