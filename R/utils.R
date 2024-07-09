


#' Improve file.path but remove duplicated separators
#'
#' @param ... passed on to base::file.path()
#'
#' @return a file path
#' @noRd
#' @keywords internal
#' @importFrom stringr str_replace_all
file.path2 = function(..., ext=NULL){
  #TODO utilise le package fs?
  fsep = .Platform$file.sep
  rtn = file.path(...) %>% str_replace_all(paste0(fsep, "+"), fsep)
  if(!is.null(ext)) rtn = paste0(rtn, ext)
  rtn
}

#' Rudimentary function to clean the names
#'
#' Avoids a dependency to janitor.
#'
#' @param string a string to clean
#' @param from the current encoding. passed on to [iconv()]. `""` is the current locale.
#'
#' @keywords internal
#' @noRd
#' @importFrom stringr str_remove_all
#' @source janitor:::old_make_clean_names(), tweaked with iconv for accents
#' @examples
#' edc_make_clean_name("àccénts")
edc_make_clean_name = function (string, from = "") {
  old_names <- string
  new_names <- old_names %>% gsub("'", "", .) %>% gsub("\"", "", .) %>% gsub("%", "percent", .) %>% 
    gsub("^[ ]+", "", .) %>% make.names(.) %>% gsub("[.]+", "_", .) %>% gsub("[_]+", "_", .) %>% 
    tolower(.) %>% gsub("_$", "", .) %>% iconv(from = from, to = "ASCII//TRANSLIT") %>% 
    str_remove_all("[\r\n]")
  dupe_count <- vapply(seq_along(new_names), function(i) {sum(new_names[i] == new_names[1:i])}, 
                       integer(1))
  new_names[dupe_count > 1] <- paste(new_names[dupe_count > 1], dupe_count[dupe_count > 1], sep = "_")
  new_names
}



#' Parse a file name to get the date of data extraction
#'
#' @param x a file
#' @noRd
#' @keywords internal
#' @importFrom stringr str_match
parse_file_datetime = function(x){
  x %>% 
    basename() %>% 
    str_match("SAS_XPORT_(\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2})") %>% 
    .[,2] %>%
    strptime(format="%Y_%m_%d_%H_%M") %>% 
    as.POSIXct()
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


#' Change a `try-error` column to a simpler character column of class "edc_error_col"
#' @noRd
#' @keywords internal
#' @importFrom dplyr across mutate
#' @importFrom tidyselect where
flatten_error_columns = function(df){
  df %>% 
    mutate(across(where(~inherits(.x, "try-error")), ~{
      attr(.x, "condition")$message %>% `class<-`("edc_error_col")
    }))
}


#' Get the date of data extraction from the modification time of all files
#'
#' @param folder a folder
#' @return date as a POSIXct scalar
#' @noRd
#' @keywords internal
#' @importFrom cli cli_warn
#' @importFrom dplyr count slice_max
get_folder_datetime = function(folder, verbose=TRUE){
  mtime=NULL
  rtn = dir(folder, full.names=TRUE) %>% file.info() %>% count(mtime=round(mtime, "secs"))
  if(isTRUE(verbose) && nrow(rtn)>1){
    cli_warn(c("Folder {.file {folder}} contains files with different modification times. 
                    The most frequent one was returned.", 
                    i="Times: {.val {rtn$mtime}}"),
                  class="get_folder_datetime_modiftime_warning")
  }
  rtn %>% slice_max(n) %>% .[1,"mtime"]
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
  stopifnot(inherits(x, "POSIXct") || inherits(x, "Date"))
  format(x, "%Y-%m-%d %Hh%M")
}



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
#' @importFrom cli cli_warn
#' @importFrom dplyr select
#' @importFrom rlang is_error
get_data_name = function(df, crfname=getOption("edc_cols_crfname", "crfname")){
  if(is_error(df)) return(NA)
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

#' @noRd
#' @keywords internal
#' @importFrom dplyr across cur_column mutate
#' @importFrom purrr map_chr
#' @importFrom tidyselect everything
copy_label_from = function(x, from){
  if(!is.list(x)){
    from_label = attr(from, "label")
    if(is.null(from_label)) return(x)
    attr(x, "label") = from_label
    return(x)
  }
  from_labs = map_chr(from, ~attr(.x, "label") %||% NA)
  mutate(x, across(everything(), ~{
    attr(.x, "label") = from_labs[cur_column()]
    .x
  }))
}


#' @noRd
#' @keywords internal
set_label = function(x, lab){
  attr(x, "label") = lab
  x
}


#' @source gtools::mixedsort
#' @noRd
#' @keywords internal 
function (x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE, 
          numeric.type = c("decimal", "roman"), roman.case = c("upper", 
                                                               "lower", "both"), scientific = TRUE) 
{
  ord <- mixedorder(x, decreasing = decreasing, na.last = na.last, 
                    blank.last = blank.last, numeric.type = numeric.type, 
                    roman.case = roman.case, scientific = scientific)
  x[ord]
}
#' @source gtools::mixedorder
#' @noRd
#' @keywords internal
mixedorder = function (x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE, 
                       roman.case = c("upper", "lower", "both"), scientific = TRUE) 
{
  roman.case <- match.arg(roman.case)
  if (length(x) == 0) 
    return(NULL)
  if (length(x) == 1) 
    return(x)
  if (!is.character(x)) {
    return(x[order(x, decreasing = decreasing, na.last = na.last)])
  }
  delim <- "\\$\\@\\$"
  if (scientific) {
    regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)(?:(?:[eE])(?:(?:[-+]?)(?:[0123456789]+))|)))"
  }
  else {
    regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)))"
  }
  numeric <- function(x) as.numeric(x)
  nonnumeric <- function(x) ifelse(is.na(numeric(x)), toupper(x), 
                                   NA)
  x <- as.character(x)
  which.nas <- which(is.na(x))
  which.blanks <- which(x == "")
  delimited <- gsub(regex, paste(delim, "\\1", delim, sep = ""), 
                    x, perl = TRUE)
  step1 <- strsplit(delimited, delim)
  step1 <- lapply(step1, function(x) x[x > ""])
  step1.numeric <- suppressWarnings(lapply(step1, numeric))
  step1.character <- suppressWarnings(lapply(step1, nonnumeric))
  maxelem <- max(sapply(step1, length))
  step1.numeric.t <- lapply(1:maxelem, function(i) {
    sapply(step1.numeric, function(x) x[i])
  })
  step1.character.t <- lapply(1:maxelem, function(i) {
    sapply(step1.character, function(x) x[i])
  })
  rank.numeric <- sapply(step1.numeric.t, rank)
  rank.character <- sapply(step1.character.t, function(x) as.numeric(factor(x)))
  rank.numeric[!is.na(rank.character)] <- 0
  rank.character <- t(t(rank.character) + apply(matrix(rank.numeric), 
                                                2, max, na.rm = TRUE))
  rank.overall <- ifelse(is.na(rank.character), rank.numeric, 
                         rank.character)
  order.frame <- as.data.frame(rank.overall)
  if (length(which.nas) > 0) {
    if (is.na(na.last)) {
      order.frame[which.nas, ] <- NA
    }
    else if (na.last) {
      order.frame[which.nas, ] <- Inf
    }
    else {
      order.frame[which.nas, ] <- -Inf
    }
  }
  if (length(which.blanks) > 0) {
    if (is.na(blank.last)) {
      order.frame[which.blanks, ] <- NA
    }
    else if (blank.last) {
      order.frame[which.blanks, ] <- 1e+99
    }
    else {
      order.frame[which.blanks, ] <- -1e+99
    }
  }
  order.frame <- as.list(order.frame)
  order.frame$decreasing <- decreasing
  order.frame$na.last <- NA
  retval <- do.call("order", order.frame)
  return(retval)
}


max_narm = function(x, na.rm=TRUE) if(all(is.na(x))) NA else max(x, na.rm=na.rm)
min_narm = function(x, na.rm=TRUE) if(all(is.na(x))) NA else min(x, na.rm=na.rm)

add_class = function(x, value){
  class(x) = c(value, class(x))
  x
}
#' @importFrom dplyr setdiff
remove_class = function(x, value){
  class(x) = setdiff(class(x), value)
  x
}


# path = "F:/Nextcloud GR/04 - Comite Pediatrie/NIVOGLIO/Analyses/Analyse DRM"
#' @importFrom rlang check_installed
init_project = function(path){
  check_installed("usethis")
  w = setwd(path)
  on.exit(setwd(w))
  
  dir.create("data")
  dir.create("R")
  dir.create("report")
  dir.create("graph")
  
  #TODO copy RProj
  writeLines(con="R/init.R", "#init")
  writeLines(con="R/read.R", "#read")
  writeLines(con="R/report.R", "#report")
  writeLines(con="R/description.R", "#description")
  
  
  data <- list(Project = "My STUDY", 
               Package = "My STUDY", 
               Version = NULL, 
               InitialBullet = "Initial version", 
               Rmd = FALSE, on_github = FALSE, 
               github_spec = NULL)
  usethis::create_project(open=FALSE)
  usethis::use_template("project-README", "README.md", data = data, open = TRUE)
  usethis::use_template("NEWS.md", data = data, open = TRUE)
  usethis::use_blank_slate(scope="project")
  #TODO: see ?usethis::use_template for custom templates
}
