test_that("openSRM-works", {


  ThermoRawFiles <- list.files(system.file('extdata/Thermo_RAW/',
                                           package = 'sRm'),
                               full.names = TRUE)


  ShimadzuRawFiles <-
    list.files(system.file('extdata/Shimadzu_LCD/',
                           package = 'sRm'),
               full.names = TRUE)

  expect_true(isS4(openSRM(ThermoRawFiles[1], source_type = 'raw', parallel = FALSE)))
  expect_true(isS4(openSRM(ShimadzuRawFiles[1], source_type = 'lcd', parallel = FALSE)))



})
