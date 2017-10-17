#' @rdname export_to_skyline

setMethod("export_to_skyline", signature = "SRM",
          function(object, transitions = NULL) {
            skyline_tibble <-
              tibble::tibble(
                Index = object@index,
                MoleculeGroup = object@header$parent,
                PrecursorName = object@header$parent,
                PrecursorRT = NA,
                PrecursorMz = object@header$parent,
                ProductMz = object@header$product,
                PrecursorCharge = object@header$polarity,
                ProductCharge = object@header$polarity
              )  %>%  mutate(PrecursorCharge = replace(PrecursorCharge, PrecursorCharge == '-', '-1')) %>%
              mutate(ProductCharge = replace(ProductCharge, ProductCharge == '-', '-1')) %>%
              mutate(PrecursorCharge = replace(PrecursorCharge, PrecursorCharge == '+', '1')) %>%
              mutate(ProductCharge = replace(ProductCharge, ProductCharge == '+', '1'))


            if (is.null(transitions)) {
              return(skyline_tibble)
            }


            if (!is.null(transitions)) {
              name_check <-
                names(transitions) == c('name', 'rt', 'polarity', 'parent', 'product')

              if (any(name_check) == FALSE) {
                stop(deparse(substitute(transitions)), ' is incorrectly named', call. = FALSE)
              }

              trindex <-
                paste0(
                  'Q1: ',
                  transitions$parent,
                  ' --> Q3:',
                  transitions$product,
                  ' (',
                  transitions$polarity,
                  ')'
                )

              transitions_tib <-
                transitions %>% tibble::as_tibble(.) %>% tibble::add_column(., Index = trindex, .before = 'name') %>%
                select(., -c(polarity, parent, product))


              transitions_join <-
                left_join(transitions_tib, skyline_tibble, by = 'Index') %>% select(
                  .,
                  MoleculeGroup = name,
                  PrecursorName,
                  PrecursorRT = rt,
                  PrecursorMz,
                  ProductMz,
                  PrecursorCharge,
                  ProductCharge
                ) %>%
                filter(., PrecursorRT != 'NA' &
                         PrecursorName != 'NA')


              return(transitions_join)
            }
          })
