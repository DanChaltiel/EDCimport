
#' @noRd
#' @keywords internal
#' @source vctrs::`%0%`
#' @seealso https://github.com/r-lib/rlang/issues/1583
`%0%` = function (x, y) {
  if(length(x) == 0L) y else x
}

#' any_of() with case sensitivity
#' @noRd
#' @keywords internal
#' @importFrom tidyselect matches
any_of2 = function(x, ignore.case=TRUE, ...){
  matches(paste(paste0("^",x,"$"), collapse="|"), ignore.case=ignore.case, ...)
}

#' @noRd
#' @keywords internal
#' @importFrom stringr str_replace_all str_to_lower
to_snake_case <- function(str) {
  str %>%
    str_replace_all("([a-z])([A-Z])", "\\1_\\2") %>%
    str_replace_all("[^\\w\\s]", "") %>%
    str_replace_all("\\s+", "_") %>%
    str_to_lower()
}


#' `fct_relevel` to the end, without warning for missing levels
#' @noRd
#' @keywords internal
#' @importFrom forcats fct_relevel
fct_last = function(f, ...) {
  lvl = c(...)
  lvl = intersect(lvl, levels(f))
  fct_relevel(f, lvl, after = Inf)
}



#' @noRd
#' @keywords internal
percent = function(x, digits=0){
  if(all(is.na(x)|is.nan(x))) return(NA)
  stopifnot(abs(x)<=1)
  x=round(x*100, digits)
  paste0(x,"%")
}


#' @noRd
#' @keywords internal
#' @source https://github.com/r-lib/cli/issues/228#issuecomment-1453614104
#' @importFrom rlang caller_env
cli_menu <- function(prompt, not_interactive, choices, quit = integer(), .envir = caller_env()) {
  if (!interactive()) {
    cli::cli_abort(c(prompt, not_interactive), .envir = .envir)
  }
  choices <- sapply(choices, cli::format_inline, .envir = .envir, USE.NAMES = FALSE)
  
  choices <- paste0(seq_along(choices), " ", choices)
  cli::cli_inform(
    c(prompt, choices),
    .envir = .envir
  )
  
  repeat {
    selected <- readline("Selection: ")
    if (selected %in% c("0", seq_along(choices))) {
      break
    }
    cli::cli_inform("Enter an item from the menu, or 0 to exit")
  }
  
  selected <- as.integer(selected)
  if (selected %in% c(0, quit)) {
    cli::cli_abort("Quiting...", call = NULL)
  }
  selected
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

#' Locate a file in a path if it doesn't exist in the working directory
#' @noRd
#' @keywords internal
#' @importFrom cli cli_abort
#' @importFrom fs file_exists path
.locate_file = function(file, path, error=TRUE, call=parent.frame()){
  if(is.null(file)) return(NULL)
  if(file_exists(file)) return(file)
  file_inpath = path(path, file)
  if(file_exists(file_inpath)) return(file_inpath)
  if(isTRUE(error)){
        cli_abort("File {.path {file}} exists neither in working directory
                nor in {.path {path}}.", 
                  class="edc_404_file_not_found", 
                  call=call)
  }
  invisible(path)
}


#' Allow using tidyselect to select members of a list
#' Still a bit limited
#' @noRd
#' @keywords internal
#' @importFrom dplyr bind_cols select
#' @importFrom purrr map
list_select = function(x, ...){
  nm = x %>% map(~1) %>% bind_cols() %>% select(...) %>% names()
  x[nm]
}

#' arrange, but mixed
#'
#' @noRd
#' @importFrom dplyr across all_of select slice
#' @examples
#' df = data.frame(x = c("Control", "Aspirin 10mg/day", "Aspirin 50mg/day",
#'                       "Aspirin 100mg/day", "Acetomycin 100mg/day",
#'                       "Acetomycin 1000mg/day"),
#'                 y=1:6)
#' df %>% mixed_arrange(x, y)
#' df %>% mixed_arrange(y, x)
#' df %>% mixed_arrange(any_of("x"))
#' df %>% mixed_arrange(any_of("foobar"))
mixed_arrange = function(.data, ...){
  cols = .data %>% select(...) %>% names()
  if(length(cols)==0) return(.data)
  for(i in rev(cols)){
    .data = .data %>% 
      slice(across(all_of(i), mixedorder)[[1]])
  }
  .data
}


# Parse zip name ------------------------------------------------------------------------------

#' Parse a file name to get the date of data extraction
#'
#' @param x a file
#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom stringr str_match
parse_file_datetime = function(archive, warn=FALSE){
  extract_datetime = archive %>% 
    basename() %>% 
    str_match("SAS_XPORT_(\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2})") %>% 
    .[,2] %>%
    strptime(format="%Y_%m_%d_%H_%M") %>% 
    as.POSIXct()
  
  if(isTRUE(warn) && is.na(extract_datetime)){
    cli_warn(c("Extraction datetime could not be read from archive's name.", 
               x="Archive's name should contain the datetime as {.code SAS_XPORT_yyyy_mm_dd_hh_MM}", 
               i="Actual archive's name: {.val {archive}}"), 
             class="edc_tm_bad_name")
  }
  
  extract_datetime
}

#' Parse a file name to get the date of data extraction
#'
#' @param x a file
#' @noRd
#' @keywords internal
#' @importFrom stringr str_remove
parse_file_projname = function(x){
  x %>% 
    basename() %>% 
    str_remove("_.*")
}



#' Get the date of data extraction 
#' 
#' Extract it from the folder path, otherwise from the modification time of most files
#'
#' @return date as a POSIXct scalar
#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom dplyr count slice_max
get_folder_datetime = function(folder, verbose=TRUE){
  folder_datetime = extract_date(folder)
  if(!is.null(folder_datetime)){
    attr(folder_datetime, "source") = "folder_path"
    return(folder_datetime)
  }
  
  mtime = dir(folder, full.names=TRUE) %>% 
    file.info() %>% 
    count(mtime=round(mtime, "secs"), sort=TRUE)
  folder_datetime = mtime %>% slice_max(n) %>% .[1,"mtime"]
  attr(folder_datetime, "source") = "folder_modif_time"
  
  if(isTRUE(verbose)){
    extraction_date = format_ymd(folder_datetime)
    folder_good = folder %>% str_remove("/$") %>% paste0("_", extraction_date)
    cli_warn(c("Folder {.file {folder}} should identify the extraction date.", 
               i="Renaming suggestion: {.file {folder_good}/}", 
               i="The extraction date was estimated based on the last modification date of the files."),
             class="get_folder_datetime_warning")
  }
  if(isTRUE(verbose) && nrow(mtime)>1){
    cli_warn(c("Folder {.file {folder}} contains files with different modification 
               times: {.val {mtime$mtime}}.", 
               i="The most frequent one was returned: {.val {folder_datetime}}."),
             class="get_folder_datetime_modiftime_warning")
  }
  folder_datetime
}


#' Extract a datetime from any string
#' @noRd
#' @importFrom cli cli_warn
#' @importFrom dplyr last
#' @importFrom lubridate parse_date_time
#' @importFrom stringr str_extract_all str_replace_all
extract_date = function(x, fmt=NULL, warn_call=parent.frame()){
  stopifnot(length(x)==1)
  if(is.null(fmt)){
    fmt = c("ymd", "ymdHM", "ymdHMS", 
            "dmy", "dmyHM", "dmyHMS")
  }
  rtn = x %>% 
    str_extract_all("(\\d|\\W|_){8,}") %>% unlist() %>% #8+ numbers or delimiters
    str_replace_all("\\D", " ") %>% #remove non-numbers
    parse_date_time(orders=fmt, quiet=TRUE) %>% 
    na.omit()
  
  if(length(rtn)==0) return(NULL)
  
  if(length(rtn)>1){
    cli_warn(c("{.file {x}} contains multiple dates: {.val {rtn}}. The last one was used."),
             class="extract_date_multiple_warning", 
             call=warn_call)
    rtn = last(rtn)
  }
  
  rtn
}

#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom dplyr select
#' @importFrom rlang is_error
get_data_name = function(df, crfname=getOption("edc_cols_crfname", "crfname")){
  if(is_error(df) || is.null(df)) return(NA)
  if(is_edc_error(df)) return("** Error in source file **")
  sel = select(df, any_of2(crfname))
  if(!is.null(attr(df, "data_name"))){
    attr(df, "data_name")
  } else if(ncol(sel)>0){
    if(ncol(sel)>1) cli_warn("Several columns named {.val {crfname}}: {.val {names(sel)}}.")
    sel[[1]][1]
  } else {
    NA
  }
}


# Labels --------------------------------------------------------------------------------------

#' @noRd
#' @keywords internal
#' @importFrom dplyr across cur_column everything mutate
#' @importFrom purrr map_chr
copy_label_from = function(x, from){
  if(!is.list(x)){
    from_label = attr(from, "label")
    if(is.null(from_label)) return(x)
    attr(x, "label") = from_label
    return(x)
  }
  from_labs = map_chr(from, ~attr(.x, "label") %||% NA)
  mutate(x, across(everything(), ~{
    lab = unname(from_labs[cur_column()])
    if(!is.na(lab)){
      attr(.x, "label") = lab
    }
    .x
  }))
}


#' @noRd
#' @keywords internal
set_label = function(x, lab){
  attr(x, "label") = lab
  x
}

#' @noRd
#' @keywords internal
#' @importFrom purrr map map2
#' @importFrom rlang is_null
get_label = function(x, default=names(x)){
  if (is.list(x)) {
    if (is.null(default)) default = rep(NA, length(x))
    if(inherits(x, "POSIXlt")) x = as.POSIXct(x)
    lab = x %>% map(get_label) %>% map2(default, ~{
      if (is.null(.x)) .y else .x
    })
  } else {
    lab = attr(x, "label", exact=TRUE)
    if (is_null(lab)) lab = default
  }
  lab
}

#' @noRd
#' @keywords internal
#' @importFrom dplyr setdiff
remove_labels = function(x){
  if(is.null(x)) return(x)
  if(is.list(x)){
    for (each in seq_along(x)){
      x[[each]] = remove_labels(x[[each]])
    }
    return(x)
  }
  attr(x, "label") = NULL
  class(x) = setdiff(class(x), c("labelled"))
  x
}

#' @noRd
#' @keywords internal
#' @importFrom fs path_ext
#' @importFrom stats var
#' @importFrom tidyr replace_na
guess_read_function = function(file){
  ext = path_ext(file)
  if(ext=="xpt") return(haven::read_xpt)
  if(ext=="sas7bdat") return(haven::read_sas)
  if(ext=="csv"){
    first_lines = readLines(file, n=2)
    n_colons = unique(stringr::str_count(first_lines, ";"))
    n_commas = unique(stringr::str_count(first_lines, ","))
    
    if(length(n_colons)==length(n_commas)){
      if(mean(n_colons>n_commas)>0.5) return(utils::read.csv2) 
      else return(utils::read.csv)
    }
    if(length(n_colons)==1 & length(n_commas)!=1) return(utils::read.csv2)
    if(length(n_commas)==1 & length(n_colons)!=1) return(utils::read.csv)
      
    if(replace_na(var(n_colons), 0) < replace_na(var(n_commas), 0)) return(utils::read.csv2)
    return(utils::read.csv)
  }
}

# NA.RM ---------------------------------------------------------------------------------------

max_narm = function(x, na.rm=TRUE) {
  if(all(is.na(x))) {
    if(is.numeric(x)) return(NA_real_) 
    return(NA)
  }
  max(x, na.rm=na.rm)
}

min_narm = function(x, na.rm=TRUE) {
  if(all(is.na(x))) {
    if(is.numeric(x)) return(NA_real_) 
    return(NA)
  }
  min(x, na.rm=na.rm)
}

# Classes -------------------------------------------------------------------------------------

#' @noRd
#' @keywords internal
#' @examples
#' iris %>% add_attributes(data_name="Iris") %>% attributes()
add_attributes = function(x, ...){
  if(is.null(x)) return(NULL)
  structure(x, ...)
}

#' @noRd
#' @keywords internal
add_class = function(x, value){
  class(x) = unique(c(value, class(x)))
  x
}
#' @noRd
#' @keywords internal
#' @importFrom dplyr setdiff
remove_class = function(x, value){
  class(x) = setdiff(class(x), value)
  x
}

# Dates ---------------------------------------------------------------------------------------


#' @noRd
#' @keywords internal
NA_Date_ = structure(NA_real_, class = "Date")

#' @noRd
#' @keywords internal
today_ymd = function(){
  format(Sys.Date(), "%Y-%m-%d")
}


#' @noRd
#' @keywords internal
format_ymd = function(x){
  stopifnot(inherits(x, "POSIXt") || inherits(x, "Date"))
  format(x, "%Y-%m-%d")
}
#' @noRd
#' @keywords internal
format_ymdhm = function(x){
  stopifnot(inherits(x, "POSIXt") || inherits(x, "Date"))
  format(x, "%Y-%m-%d %Hh%M")
}

#' @noRd
#' @keywords internal
bad_hms = function(x){
  inherits(x, c("hms", "difftime")) && !typeof(x) %in% c("integer", "double")
}

#' TM sometimes creates hms/difftime columns of type character instead of double
#' @noRd
#' @keywords internal
fix_hms = function(x){
  if(!bad_hms(x)) return(x)
  y = as.numeric(unclass(x))
  attributes(y) = attributes(x)
  y
}
