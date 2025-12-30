
#' EDCimport Database
#' 
#' This class of object represents a database, as the result of an EDCimport reading function.
#' It has its own `print()` method.
#' 
#' @section Functions returning `edc_database` objects:
#' As per now, reading functions are: [read_trialmaster()], [read_all_sas()], [read_all_xpt()], and [read_all_csv()].
#' 
#' @section Structure:
#' While it is not usually useful to query them, an `edc_database` object is a named list containing:
#' - all the datasets from the source files
#' - `datetime_extraction` and `date_extraction` the inferred date of data extraction
#' - `.lookup` a temporary copy of the lookup table
#' 
#' @seealso [read_trialmaster()]
#' 
#' @name edc_database
#' @docType class
NULL


# Constructor ---------------------------------------------------------------------------------


#' Build and add lookup, add datetime_extraction, and add `...` as attributes
#' @noRd
#' @keywords internal
new_edc_database = function(datalist, datetime_extraction, extend_lookup=TRUE, ...){
  assert_class(datetime_extraction, c("POSIXt", "Date"))
  .lookup = build_lookup(datalist)
  if(!is.null(extend_lookup)){
    .lookup = extend_lookup(.lookup, datasets=datalist)
  }
  .lookup = .lookup %>% 
    structure(datetime_extraction=datetime_extraction,
              ...)
  
  datalist$datetime_extraction = datetime_extraction
  datalist$date_extraction = format_ymd(datetime_extraction)
  datalist$.lookup = .lookup
  
  class(datalist) = "edc_database"
  
  datalist
}




# Methods -----------------------------------------------------------------

#' @export
#' @importFrom cli cat_rule cli_bullets cli_vec
#' @importFrom purrr discard_at keep
print.edc_database = function(x, ...){
  x = x %>% keep(is.data.frame) %>% discard_at(".lookup")
  cat_rule("EDCimport database", col = "violet")
  nms = cli_vec(names(x), list("vec-trunc"=3))
  cli_bullets(c(
    "Contains {length(x)} table{?s}: {.arg {nms}}",
    i="Use {.run EDCimport::load_database(db)} to load the tables in the global environment.",
    i="Use {.run EDCimport::edc_lookup()} to see the summary table."
  ))
}

#' @export
print.tm_database = print.edc_database
