test_that("sRm-methods", {
  ShimadzuRawFiles <-
    list.files(system.file('extdata/Shimadzu_LCD/',
                           package = 'sRm'),
               full.names = TRUE)


  TestObject <- openSRM(ShimadzuRawFiles, source_type = 'lcd', parallel = FALSE)
  expect_false(tibble::is_tibble(TestObject@peaks))


  TestObjectPks <-
    detectPeaks(TestObject,
                snthresh = 100,
                peakwidth = c(10, 30), parallel = FALSE)

  expect_true(tibble::is_tibble(TestObjectPks@peaks))
  expect_false('GroupID' %in% names(TestObjectPks@peaks))

  TestObjectGrp <- groupPeaks(TestObjectPks, rt_tolerance = 2.0)
  expect_true('GroupID' %in% names(TestObjectGrp@peaks))

  TestSummary <- groupSummary(TestObjectGrp)
  expect_true(tibble::is_tibble(TestSummary))


  TestFilter <- keepTransitions(TestObject, index_keep = c(1:5))
  expect_true(nrow(transitions(TestFilter)) == 5)
  expect_true(nrow(transitions(TestFilter)) < nrow(transitions(TestObject)))


  TestFilterSample <- removeSample(TestObject, 'QC01')
  expect_true(nrow(TestFilterSample@chroms) < nrow(TestObject@chroms))
  expect_true(nrow(TestFilterSample@header) < nrow(TestObject@header))


})
