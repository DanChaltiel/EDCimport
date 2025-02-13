



#' Clean up the names of all datasets
#' 
#' Clean the names of all the datasets in the database, so that columns are only lowercase letters, 
#' numbers, and `_`.
#' 
#' @param database an [edc_database] object, from [read_trialmaster()] or other EDCimport reading functions.
#' @return an [edc_database] object
#' 
#' @importFrom dplyr rename_with
#' @importFrom purrr map_if
#' @export
#' 
#' @examples
#' #db = read_trialmaster("filename.zip", pw="xx")
#' db = edc_example() %>% 
#'   edc_clean_names()
#' names(db$enrol)
edc_clean_names = function(database){
  .lookup = database$.lookup
  database = database %>% 
    map_if(~is.data.frame(.x) && !inherits(.x, "edc_lookup"),
           ~.x %>% rename_with(edc_make_clean_name))

  database$.lookup = database %>% 
    build_lookup() %>% 
    extend_lookup(datasets=database)
  
  .set_lookup(database$.lookup, verbose=FALSE)
  
  database
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
