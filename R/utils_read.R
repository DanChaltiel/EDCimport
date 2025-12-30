

#' Read all files using a specific read function, returning a named list of tibbles
#' 
#' @section Collision risk: 
#' We use 8 hex characters (4*8=32 bits) to identify the cache. The risk of
#' collision for `x` cache updates is `1-(1-(1/2^32))^x`, so 0.023% for 1e6 updates.
#' 
#' @param ... passed to `read_function`
#' @noRd
#' @keywords internal
#' @importFrom dplyr across as_tibble mutate where
#' @importFrom fs path_ext_remove
#' @importFrom purrr map pluck
#' @importFrom rlang hash hash_file set_names
#' @importFrom stringr fixed str_remove str_replace_all
.read_all = function(files, read_function, ..., path=NULL, 
                     use_cache, clean_names_fun=NULL, verbose){
  assert_file_exists(files)
  clean_names_fun = .get_clean_names_fun(clean_names_fun)
  if(!is.null(path)){
    path = normalizePath(path, winslash="/")
    file_names = files %>% normalizePath(winslash="/") %>% 
      str_remove(fixed(as.character(path))) %>% str_remove("^/") %>% 
      str_replace_all("/", "_") %>% tolower() %>% path_ext_remove()
  } else {
    file_names = basename(files) %>% tolower() %>% path_ext_remove()
  }
  subjid_cols = get_subjid_cols(lookup=NULL)
  crf = get_crfname_cols(lookup=NULL)
  
  f_hash = files %>% sort() %>% map_chr(~hash_file(.x)) %>% hash()
  f_hash8 = f_hash %>% str_sub(1, 8)
  cache_file = glue("{path}/EDCimport_cache_{f_hash8}.rds")
  read_from_cache = file_exists(cache_file) && (isTRUE(use_cache) || use_cache=="read")
  if(read_from_cache){
    if(verbose>0) cli_inform("Reading cache: {.file {cache_file}}", class="read_tm_cache")
    rtn = readRDS(cache_file) %>% 
      structure(source="cache")
    
    cache_version = attr(rtn, "EDCimport_version")
    cache_outdated = packageVersion("EDCimport") > cache_version
    cache_invalid = attr(rtn, "hash") != f_hash
    if(cache_outdated || cache_invalid){
      cli_inform(c(i="Updating cache with latest {.pkg EDCimport} version
                      (v{cache_version} to v{packageVersion('EDCimport')})"), 
                 class="edc_read_from_cache_outdated")
    } else {
      return(rtn)
    }
  }
  
  rtn = files %>% 
    set_names(file_names) %>% 
    map(function(.x) {
      tbl = tryCatch(read_function(.x, ...), 
                     error = function(e) .flatten_error(e, class="edc_error_data"))
      if(is_edc_error(tbl)) return(tbl)
      label = tbl %>% select(any_of2(crf)) %>% pluck(1, 1)
      rtn = tbl %>% 
        as_tibble() %>% 
        clean_names_fun() %>% 
        arrange(pick(any_of2(subjid_cols))) %>%
        mutate(across(where(bad_hms), fix_hms))
      attr(rtn, "hash") = hash(rtn)
      attr(rtn, "label") = label
      rtn
    }) %>% 
    structure(source="files", 
              EDCimport_version=packageVersion("EDCimport"),
              hash=f_hash)
  
  if(isTRUE(use_cache) || use_cache=="write"){
    if(verbose>0) cli_inform("Writing cache {.file {cache_file}}", class="edc_create_cache")
    saveRDS(rtn, cache_file)
    if(!file_exists(cache_file)) {
      cli_warn("Could not save cache file", class="read_tm_zip_no_cache_warning")
    }
  }
  
  rtn
}


#' Apply `.flatten_error` to all `try-error` columns
#' @noRd
#' @keywords internal
#' @importFrom dplyr across mutate where
.flatten_error_columns = function(df){
  df %>% 
    mutate(across(where(~inherits(.x, "try-error")), .flatten_error))
}


#' Change a `try-error` into a simpler character column of class "edc_error_col"
#' @noRd
#' @keywords internal
#' @importFrom purrr map_chr
#' @importFrom rlang error_cnd
#' @importFrom stringr str_detect
.flatten_error = function(e, class="edc_error_col"){
  if(!inherits(e, c("try-error", "error"))) return(e)
  if(inherits(e, c("try-error"))) e = attr(e, "condition")
  e$trace=NULL
  if(str_detect(e$message, "Corrupt `?Date`? with unknown type character")){
    m = c("CRF error: some date-type columns are stored as text.",
          i="Solution: in Trialmaster, set the table attribute to 'Custom' and 
            fix the date formats (char -> num / 8. / date9.).",
          i="See the {.vignette [Table Reading Date Error](EDCimport::trialmaster_date_error)} vignette for guidance.")
    m = map_chr(m, format_inline)
    e = error_cnd(message=m, use_cli_format=TRUE)
  }
  e
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort
#' @importFrom rlang as_function is_formula
.get_clean_names_fun = function(f){
  if(is.null(f)) return(identity)
  if(is_formula(f)) f = as_function(f)
  if(!is.function(f)) cli_abort("{.arg {caller_arg(f)}} should be a function or a lambda-function, not a {.cls {class(f)}}.")
  f
}


#' clean all names using `clean_names_fun`, or do nothing if `NULL`
#' @noRd
#' @keywords internal
#' @importFrom purrr map
.clean_names = function(datalist, clean_names_fun){
  #TODO: merge with .get_clean_names_fun()
  clean_names_fun = .get_clean_names_fun(clean_names_fun)
  datalist %>% map(clean_names_fun)
}


#' clean all labels for non-UTF8 characters
#' @noRd
#' @keywords internal
#' @importFrom purrr modify_if modify
.clean_labels_utf8 = function(datalist, warn=FALSE){
  datalist %>% 
    modify_if(is.data.frame, function(df){
      df %>% modify(~{
        attr(.x, "label") = .repair_invalid_utf8(attr(.x, "label"))
        .x
      })
    })
}


#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom dplyr coalesce
#' @importFrom glue glue
#' @importFrom purrr possibly
#' @importFrom rlang set_names
#' @importFrom stringr str_sub
.repair_invalid_utf8 = function(x, warn=FALSE){
  bad = is_invalid_utf8(x)
  if(!any(bad)) return(x)
  #most of the time, it is just the last character
  x2 = ifelse(bad, str_sub(x, end=-2), x)
  if(!any(is_invalid_utf8(x2))) return(x2)
  
  if(any(bad, na.rm=TRUE)){
    #try main encodings
    iconv2 = possibly(iconv, otherwise=NA)
    xbad2 = iconv2(x[bad], from="", to="UTF-8")
    xbad3 = iconv2(x[bad], from="latin1", to="UTF-8")
    xbad4 = iconv2(x[bad], from="ASCII", to="UTF-8")
    xbad5 = iconv2(x[bad], from="ASCII/TRANSLIT", to="UTF-8")
    xgood = coalesce(xbad2, xbad3, xbad4, xbad5)
    x[bad] = xgood
    
    if(isTRUE(warn)){
      bad_utf8 = glue("{x$dataset}${x$names} ({x$valid_labels}) ") %>% set_names("i")
      cli_warn(c("Found {length(bad_utf8)} invalid UTF-8 label{?s}:", bad_utf8))
    }
  }
  x
}
