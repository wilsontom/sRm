#' Open PRM Files
#'
#' Open and parse PRM files into an `S4` PRM Object
#'
#' @param files a character vector of absolute file paths of PRM files in `.mzML` format
#' @return a PRM object
#' @export

openPRM <- function(files)
{

  # map over input files and open with mzR
  opentmp <- purrr::map(files, ~ {
    mzR::openMSfile(., backend = "pwiz")
  })


  # map over inputs and extract spectra
  pkstmp <- purrr::map(opentmp, ~ {
    mzR::peaks(.)
  })

  # extract spectra headers. this replaces old xml2 parsing code. Most cvParams are now supported by mzR
  file_hdrs <- purrr::map(opentmp, ~ {
    mzR::header(.) %>% dplyr::filter(msLevel == 2)
  })

  # dump out all MS1 spectra
  ms2spec <- list()
  for (i in seq_along(file_hdrs)) {
    ms2spec[[i]] <-
      pkstmp[[i]][file_hdrs[[i]][, 'acquisitionNum']]
  }

  # iterate over MS2 spectra and add in rt
  for (i in seq_along(ms2spec)) {
    for (k in seq_along(ms2spec[[i]])) {
      ms2spec[[i]][[k]] <-
        ms2spec[[i]][[k]] %>% data.frame() %>% tibble::add_column(.,
                                                                  rt = file_hdrs[[i]]$retentionTime[k],
                                                                  precursprMZ = file_hdrs[[i]]$precursorMZ[k])
      names(ms2spec[[i]][[k]]) <- c('mz', 'int', 'rt')
    }
  }

  # iterate over chromatogram tibbles and add fileName as sample identifier
  for (i in seq_along(ms2spec)) {
    ms2spec[[i]] <-
      purrr::map(ms2spec[[i]], ~ {
        dplyr::mutate(., sampleID = basename(files[i]))
      })

  }
























}
