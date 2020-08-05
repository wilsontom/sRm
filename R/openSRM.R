#' Open SRM Files
#'
#' Open and parse SRM files into an `S4` SRM Object
#'
#' @param files a character vector of absolute file paths of SRM files in `.mzML` format
#' @param source_type a character string of the original file format (`raw` or `lcd`)
#' @return an SRM object
#' @export
#' @importFrom magrittr %>%

openSRM <- function(files, source_type)
{
  # map over input files and open with mzR
  opentmp <- purrr::map(files, ~ {
    mzR::openMSfile(., backend = "pwiz")
  })

  # map over inputs and extract chromatograms
  chromtmp <- purrr::map(opentmp, ~ {
    mzR::chromatogram(.)
  })

  # extract chromatogram headers. this replaces old xml2 parsing code. Most cvParams are now supported by mzR
  file_hdrs <- purrr::map(opentmp, ~ {
    mzR::chromatogramHeader(.)
  })

  # extract transition names from the chromatograms
  transition_names <- list()
  for (i in seq_along(chromtmp)) {
    transition_names[[i]] <-
      purrr::map_chr(chromtmp[[i]], ~ {
        names(.)[[2]]
      })
  }

  # iterate over files + chromatograms and convert rt-int matrices to named tibbles
  chromtib <- list()
  for (i in seq_along(chromtmp)) {
    chromtib[[i]] <-
      purrr::map(chromtmp[[i]], tibble::as_tibble, validate = FALSE) %>% purrr::map(., dplyr::select, rt = 1, int = 2)
  }


  # iterate over chromatogram tibbles and add fileName as sample identifier
  for (i in seq_along(chromtib)) {
    chromtib[[i]] <-
      purrr::map(chromtib[[i]], ~ {
        dplyr::mutate(., sampleID = basename(files[i]))
      })

  }

  # iterate over chromatogram tibbles and add transition name as identifier

  for (i in seq_along(chromtib)) {
    for (k in seq_along(chromtib[[i]])) {
      chromtib[[i]][[k]] <-
        chromtib[[i]][[k]] %>% tibble::add_column(., index = file_hdrs[[i]]$chromatogramId[k])
    }
  }


  # create a long format tibble of all the peak data
  peak_table <-
    unlist(chromtib, recursive = FALSE) %>% dplyr::bind_rows()

  # remove file extentions
  peak_table$sampleID <-
    stringr::str_remove_all(peak_table$sampleID, '.mzML')


  object <- new("SRM")
  object@rawChrom <- peak_table


  # checksum <-
  #   purrr::map_chr(files, openssl::md5) %>% tibble(sampleID = basename(files), MD5 = .)

  names(file_hdrs) <- basename(files)


  # create a tidy tibble of file headers
  file_hdrs_clean <-
    purrr::map(file_hdrs, ~ {
      tibble::tibble(
        index = .$chromatogramId,
        polarity = .$polarity,
        Q1 = .$precursorIsolationWindowTargetMZ,
        Q3 = .$productIsolationWindowTargetMZ
      )
    })

  for (i in seq_along(file_hdrs_clean)) {
    file_hdrs_clean[[i]] <-
      tibble::add_column(
        file_hdrs_clean[[i]],
        sampleID = stringr::str_remove_all(basename(files[[i]]), '.mzML'),
        .before = 'index'
      )
  }

  file_hdrs_clean <-
    file_hdrs_clean %>% dplyr::bind_rows() %>% dplyr::mutate(polarity = replace(polarity, polarity == 0, '-')) %>%
    dplyr::mutate(polarity = replace(polarity, polarity == 1, '+')) %>%
    dplyr::mutate(polarity = replace(polarity, polarity == -1, 'TIC'))

    clean_transition <- unlist(format_scan_header(file_hdrs_clean))

    file_hdrs_clean <- file_hdrs_clean %>% dplyr::mutate(transition = clean_transition) %>%
    dplyr::filter(index != 'TIC')

  object@transitions <-
    file_hdrs_clean %>% dplyr::select(transition, index) %>% dplyr::distinct() %>% dplyr::filter(index != 'TIC')

  object@rawChrom <-
    object@rawChrom %>% dplyr::filter(index != 'TIC')

  object@transitions <-
    object@transitions %>% dplyr::mutate(index_n = seq(from = 1, to = nrow(.)))

  tic_bpi <-
    object@rawChrom %>% dplyr::group_by(sampleID, index) %>% dplyr::summarise(tic = sum(int), bpi = max(int)) %>% dplyr::ungroup()


  object@header <-
    dplyr::full_join(tic_bpi, file_hdrs_clean, by = c('sampleID', 'index'))

  meta_tibble <-
    purrr::map(files, ~ {
      get_meta(., type = source_type)
    }) %>% purrr::map(., ~ {
      tidyr::spread(., name, value)
    }) %>% dplyr::bind_rows() %>% dplyr::mutate(sample_n = seq(from = 1, to = nrow(.))) %>%
    dplyr::select(sampleID, sample_n, Datestamp, Timestamp, Instrument, Schema)


  meta_ext <- tools::file_ext(meta_tibble$sampleID)

  meta_tibble$sampleID <-
    stringr::str_remove_all(meta_tibble$sampleID, paste0('.', meta_ext))
  object@meta <- meta_tibble


  return(object)

}
