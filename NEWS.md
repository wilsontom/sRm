### NEWS


#### v0.1.0
  - First stable release
  - Reading and parsing of Thermo TSQ Vantage SRM data
  - Single function for reading and decoding peaks; stored to S4 object
  - S4 method for combining all transitions from unique parent (Q1) m/z value
  - S4 methods for viewing object: show, meta, transitions
  - Basic tests (testthat + codecov)
  - Travis and AppVeyor Continuous Integration
  - Passes R CMD check with no ERRORS or WARNINGS

#### v0.1.1
  - Adapt `combineTransitions` to catch SRM's of different lengths
  - Result of `combineTransitions` is now S3 class
  - Plot method for `combineTransitions` class ;(`plotAll`)
  - Plot method for single transition events; (`plotSRM`)
  - Bug fixed for when time and intensity arrays are encoded with different precisions
  - `grid` and `gridExtra` are listed as Dependencies

#### v0.1.2
  - Fix `no visible binding' NOTE for 'ggplot2`
  - Update manual
  - Increase test coverage

#### v0.1.3
 - Add `plotMulti` for easy graphical representation of complex SRM-MS chromatograms
 - Cleaned up filter naming throughout
 - Fixed CI build environment, so there is no longer a list of unnecessary Imports in DESCRIPTION
 - `openSRMfile` wasn't checking for file compression and assuming that file were converted with no compression. `openSRMfile` now checks for `zlib` compression and if found passes the `gzip` compression type arg to `sRm::decodePeaks`
 - Instrument, Instrument Model and Instrument Serial are all separate fields in the `meta` method
 - Internal helper function add (`scanPolarity`) to extract scan polarity from `cvParam` and insert into `object@header`
 - Parent (Q1) and product (Q3) masses are extracted from `cvParams` using `getQMZs` and are no longer string processed from filter names
 - `filter` slot added to the `SRM` class. This contains a raw copy of `idRefs`
 - `object@header` now includes scan polarity; `-1 = negative, 1 = positive`
 - `totalIonCount` has become `totIonCount` to improve readability
 - `combineTransitions` now does a "best effort" at checking for transitions which have been acquired using dynamic MRM mode
    - In these circumstances; a transition set needs to consider the retention window aswell as the parent m/z
    - Unique filter ID strings are created from parsed `idRefs`
    - Where Rt lengths are unequal within unique transitions set; these are (for now) removed from the `transitions` class with a on-screen message stating which `idRefs` have been dropped
