# sRm [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![Build Status](https://travis-ci.org/wilsontom/sRm.svg?branch=master)](https://travis-ci.org/wilsontom/sRm) [![Build status](https://ci.appveyor.com/api/projects/status/hd7toi1bcfxchiua/branch/master?svg=true)](https://ci.appveyor.com/project/wilsontom/srm/branch/master) [![codecov](https://codecov.io/gh/wilsontom/sRm/branch/master/graph/badge.svg)](https://codecov.io/gh/wilsontom/sRm) [![License](https://img.shields.io/badge/license-GNU%20GPL%20v3.0-blue.svg "GNU GPL v3.0")]


> __a minimal parser for selective reaction monitoring (SRM) mass spectrometry (MS) data__


`sRm` is a basic parser for selective reaction monitoring (SRM) mass spectrometry (MS) data. SRM-MS data which is converted to chromatogram data during `RAW` to `mzML` conversion, is  parsed and decoded, then stored in a simple `S4` object for easy access.

This package has __only__ been tested using SRM-MS data acquired on a __Thermo Scientific TSQ Quantum Ultra (QQQ) Mass Spectrometer__. Following acquisition, `RAW` data was converted to `mzML` using [`msconvert`](http://proteowizard.sourceforge.net/tools.shtml).


#### Installation & Usage

```R
devtools::install_github("wilsontom/sRm")
```

```R
library(sRm)
SRMdata <- openSRMfile("example_file.mzML")

# View sample information
SRMdata
file id : Calibrant-Mix-D2

run time :  19.02 mins

78 SRM transitions measured

26 unique parent masses

78 unique Q3 product ions

# View break down of SRM transitions
transitions(SRMdata)
Q1: 136.055 -> Q3: 65.014/80.292/108.19
Q1: 153.01 -> Q3: 65.271/67.232/109.094
Q1: 163.051 -> Q3: 91.242/93.114/119.137
Q1: 164.029 -> Q3: 72.277/77.266/105.234
Q1: 167.085 -> Q3: 69.384/124.185/150.13
Q1: 167.096 -> Q3: 94.22/96.302/124.152

# View minimal amount of meta data
meta(SRMdata)
file id : Calibrant-Mix-D2

Instrument : TQU01681

Precision : 64-bit float

Compression : no compression

Conversion scheme:
http://psi.hupo.org/ms/mzmlhttp://psidev.info/files/ms/mzML/xsd/mzML1.1.0.xsd
````
