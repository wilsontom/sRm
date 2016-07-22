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
