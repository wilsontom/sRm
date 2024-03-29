test_that("sRm-methods", {
  ShimadzuRawFiles <-
    list.files(system.file('extdata/Shimadzu_LCD/',
                           package = 'sRm'),
               full.names = TRUE)


  TestObject <- openSRM(ShimadzuRawFiles)
  expect_false(tibble::is_tibble(TestObject@peaks))


  TestObjectPks <-
    detectPeaks(TestObject,
                snthresh = 100,
                peakwidth = c(10, 30),
                parallel = FALSE)
  expect_true(tibble::is_tibble(TestObjectPks@peaks))
  expect_false('GroupID' %in% names(TestObjectPks@peaks))

  TestObjectGrp <- groupPeaks(TestObjectPks, rt_tolerance = 2.0)
  expect_true('GroupID' %in% names(TestObjectGrp@peaks))

  TestSummary <- groupSummary(TestObjectGrp)
  expect_true(tibble::is_tibble(TestSummary))



})
