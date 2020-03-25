### NEWS

#### v0.2.0

**Breaking Changes**

- Multiple file input. `SRM` objects can be used for single `mzML` files or batch mode for multiple files
- Basic support for peak smoothing (Savitzky-Golay implemented at present)
- Support for chromatogram feature detection (pracma & xcms matchedFilter)
- Improved plotting methods
- Complete re-write of `openSRM` function and all underlying methods
- All previous methods form v0.1.4 and lower; now deprecated

#### v0.1.4
 - The main change is that extraction and decoding of chromatogram time and intensity arrays is now performed by `mzR`.
 - `xml2` is still used for extraction of header data which is currently unsupported by `mzR`
 - A lot of the internal workings have been updated / replaced with more efficent approaches; mainly utilising `dplyr`, `tibble` and `purrr`
 - New method `export_to_skyline` exports SRM transitions in a format that can saved as `.csv` and uploaded to [Skyline](https://skyline.ms/project/home/software/Skyline/begin.view)
 - The `peaks` slot in `SRM-object` is now a list of tibbles
 - `index` slot in `SRM-object` incorporates polarity (- or +)
 - `SRM-object` `header`is now a tibble
 - Bug fix in `plotAll` which caused multiple outputs during layering of plots
 - Bug fix in `plotMulti` which caused switching of colours between `geom_lines` and labels
 - Bug fix in `plotAll` which was causing for a new index order to be created
 - Additional labelling options (default or custom) for `plotMulti`
 - Added vignette
 

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

#### v0.1.2
  - Fix `no visible binding' NOTE for 'ggplot2`
  - Update manual
  - Increase test coverage
  
#### v0.1.1
  - Adapt `combineTransitions` to catch SRM's of different lengths
  - Result of `combineTransitions` is now S3 class
  - Plot method for `combineTransitions` class ;(`plotAll`)
  - Plot method for single transition events; (`plotSRM`)
  - Bug fixed for when time and intensity arrays are encoded with different precisions
  - `grid` and `gridExtra` are listed as Dependencies

#### v0.1.0
  - First stable release
  - Reading and parsing of Thermo TSQ Vantage SRM data
  - Single function for reading and decoding peaks; stored to S4 object
  - S4 method for combining all transitions from unique parent (Q1) m/z value
  - S4 methods for viewing object: show, meta, transitions
  - Basic tests (testthat + codecov)
  - Travis and AppVeyor Continuous Integration
  - Passes R CMD check with no ERRORS or WARNINGS
