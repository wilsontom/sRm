#' Open SRM Files
#'
#' Open and parse SRM files into an `S4` SRM Object
#'
#' @param files a character vector of absolute file paths of SRM files in `.mzML` format
#' @return an SRM object
#' @export
#' @importFrom magrittr %>%

openSRM <-
  function(files)
  {
    # map over input files and open with MSnbase
    opentmp <- furrr::future_map(files, ~ {
      MSnbase::readSRMData(.)
    })


    get_chroms <- function(x)
    {
      idx_range <- 1:length(x)

      chrom <- purrr::map(idx_range, ~ {
        cbind(x[., ]@rtime, x[., ]@intensity)
      })

      chromID <- MSnbase::fData(x)[['chromatogramId']]

      for (i in seq_along(chrom)) {
        names(chrom)[[i]] <- chromID[i]
      }

      return(chrom)
    }


    chromtmp <- furrr::future_map(opentmp, get_chroms)


    file_hdrs <- furrr::future_map(opentmp, ~ {
      MSnbase::fData(.)
    })

    # iterate over files + chromatograms and convert rt-int matrices to named tibbles
    chromtib <- list()
    for (i in seq_along(chromtmp)) {
      chromtib[[i]] <-
        furrr::future_map(chromtmp[[i]], tibble::as_tibble, .name_repair = 'minimal') %>% purrr::map(., dplyr::select, rt = 1, int = 2)
    }


    # iterate over chromatogram tibbles and add fileName as sample identifier
    for (i in seq_along(chromtib)) {
      chromtib[[i]] <-
        furrr::future_map(chromtib[[i]], ~ {
          dplyr::mutate(.,
                        sampleID = tools::file_path_sans_ext(basename(files[[i]]), compression = TRUE))
        })

    }


    # iterate over chromatogram tibbles and add transition name as identifier

    for (i in seq_along(chromtib)) {
      for (k in seq_along(chromtib[[i]])) {
        chromtib[[i]][[k]] <-
          chromtib[[i]][[k]] %>% tibble::add_column(., filter = file_hdrs[[i]]$chromatogramId[k])
      }
    }


    # create a long format tibble of all the peak data
    peak_table <-
      unlist(chromtib, recursive = FALSE) %>% dplyr::bind_rows()

    # remove file extensions
    peak_table <- dplyr::mutate(peak_table,
                                sampleID = tools::file_path_sans_ext(basename(sampleID), compression = TRUE))

    object <- new("SRM")
    object@chroms <- peak_table

    names(file_hdrs) <- basename(files)

    # create a tidy tibble of file headers
    file_hdrs_clean <-
      furrr::future_map(file_hdrs, ~ {
        tibble::tibble(
          filter = .$chromatogramId,
          polarity = .$polarity,
          Q1 = .$precursorIsolationWindowTargetMZ,
          Q3 = .$productIsolationWindowTargetMZ,
          CE = .$precursorCollisionEnergy
        )
      })

    for (i in seq_along(file_hdrs_clean)) {
      file_hdrs_clean[[i]] <-
        tibble::add_column(
          file_hdrs_clean[[i]],
          sampleID = tools::file_path_sans_ext(basename(files[[i]]), compression = TRUE),
          .before = 'filter'
        )
    }

    file_hdrs_clean <-
      file_hdrs_clean %>% dplyr::bind_rows() %>% dplyr::mutate(polarity = replace(polarity, polarity == 0, '-')) %>%
      dplyr::mutate(polarity = replace(polarity, polarity == 1, '+')) %>%
      dplyr::mutate(polarity = replace(polarity, polarity == -1, 'TIC'))

    clean_transition <- unlist(format_scan_header(file_hdrs_clean))

    file_hdrs_clean <-
      file_hdrs_clean %>% dplyr::mutate(transition = clean_transition) %>%
      dplyr::filter(filter != 'TIC')

    object@transitions <-
      file_hdrs_clean %>% dplyr::select(transition, filter) %>% dplyr::distinct() %>% dplyr::filter(filter != 'TIC')

    object@chroms <-
      object@chroms %>% dplyr::filter(filter != 'TIC')



    InstrumentModel <- detectInstrumentModel(files[1])

    if (stringr::str_detect(InstrumentModel, 'Shimadzu')) {
      object@chroms$rt <- object@chroms$rt / 60
    }

    object@transitions <-
      object@transitions %>% dplyr::mutate(index = seq(from = 1, to = nrow(.)))

    tic_bpi <-
      object@chroms %>% dplyr::group_by(sampleID, filter) %>% dplyr::summarise(tic = sum(int), bpi = max(int)) %>% dplyr::ungroup()

    object@header <-
      dplyr::full_join(tic_bpi, file_hdrs_clean, by = c('sampleID', 'filter'))

    meta_tibble <-
      furrr::future_map(files, ~ {
        fileMetaData(.)
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
