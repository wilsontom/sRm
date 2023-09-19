test_that("openSRM-works", {


  ThermoRawFiles <- list.files(system.file('extdata/Thermo_RAW/',
                                           package = 'sRm'),
                               full.names = TRUE)


  ShimadzuRawFiles <-
    list.files(system.file('extdata/Shimadzu_LCD/',
                           package = 'sRm'),
               full.names = TRUE)

  expect_true(isS4(openSRM(ThermoRawFiles[1])))
  expect_true(isS4(openSRM(ShimadzuRawFiles[1])))



})
