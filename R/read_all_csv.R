

#' Read all `.csv` files in a directory
#' 
#' Read all `.csv` files in a directory, with labels if specified.
#'
#' @param path \[`character(1)`]\cr path to the directory containing `.csv` files.
#' @param ... unused
#' @param labels_from \[`character(1)`]\cr path to the file containing the labels. See section "Labels file" below.
#' @param read_fun \[`function`]\cr if "guess" doesn't work properly, a function to read the files in path, e.g. `read.csv`, `read.csv2`,...
#' @param clean_names_fun `r lifecycle::badge("deprecated")` use [edc_clean_names()] instead.
#' @inheritParams read_all_xpt
#'
#' @section Labels file: 
#' `labels_from` should contain the information about column labels. It should be a data file (`.csv`) containing 2 columns: one for the column name and the other for its associated label. Use `options(edc_col_name="xxx", edc_col_label="xxx")` to specify the names of the columns.
#' 
#' @inheritSection read_all_xpt Format file
#'  
#' @return a list containing one dataframe for each `.csv` file in the folder, the extraction date (`datetime_extraction`), and a summary of all imported tables (`.lookup`).
#' @family EDCimport reading functions
#' 
#' @export
#' @importFrom fs dir_ls is_dir
#' @importFrom rlang check_dots_empty
#' @importFrom utils packageVersion
#' 
#' @examples
#' # Create a directory with multiple csv files and a label lookup.
#' path = paste0(tempdir(), "/read_all_csv")
#' dir.create(paste0(path, "/subdir"), recursive=TRUE)
#' write.csv(iris, paste0(path, "/iris.csv"))
#' write.csv(mtcars, paste0(path, "/mtcars.csv"))
#' write.csv(mtcars, paste0(path, "/subdir/mtcars.csv"))
#' write.csv(airquality, paste0(path, "/airquality.csv"))
#' labs = c(iris, mtcars, airquality) %>% names()
#' write.csv(data.frame(name=labs, label=toupper(labs)), paste0(path, "/labels.csv"))
#' 
#' 
#' db = read_all_csv(path, labels_from="labels.csv", subdirectories=TRUE) %>% 
#'   set_project_name("My great project")
#' db
#' edc_lookup()
read_all_csv = function(path, ..., 
                        labels_from=NULL,
                        format_file=NULL, 
                        subdirectories=FALSE,
                        read_fun="guess", 
                        datetime_extraction="guess", 
                        verbose=getOption("edc_read_verbose", 1),
                        clean_names_fun=NULL){
  check_dots_empty()
  reset_manual_correction()
  assert(is_dir(path))
  
  if(identical(datetime_extraction, "guess") || is.null(datetime_extraction)){
    datetime_extraction = get_folder_datetime(path, verbose=verbose)
  }  
  assert_class(datetime_extraction, c("POSIXt", "Date"))
  
  if(identical(read_fun, "guess")){
    files = dir_ls(path, regexp="\\.csv")
    read_fun = guess_read_function(files[1])
  }
  assert_class(read_fun, c("function"))

  clean_names_fun = .get_clean_names_fun(clean_names_fun)
  rtn = dir_ls(path, regexp="\\.csv", recurse=subdirectories) %>% 
    .read_all(read_fun, clean_names_fun=clean_names_fun, path=path) %>% 
    .add_labels(labels_file=labels_from, path, read_fun) %>% 
    .apply_sas_formats(format_file) %>%
    .add_lookup_and_date(
      datetime_extraction=datetime_extraction,
      clean_names_fun=clean_names_fun, 
      EDCimport_version=packageVersion("EDCimport")
    )
  
  .set_lookup(rtn$.lookup)
  
  class(rtn) = "edc_database"
  rtn
}


#' @noRd
#' @keywords internal
#' @importFrom fs path path_ext_remove
#' @importFrom purrr map
.add_labels = function(datalist, labels_file, path, read_fun){
  if(is.null(labels_file)) return(datalist)
  
  label_df_name = path_ext_remove(basename(labels_file))
  if(label_df_name %in% names(datalist)) {
    data_labels = datalist[[label_df_name]]
    datalist[[label_df_name]] = NULL
  } else {
    if(!file.exists(labels_file)) labels_file = path(path, labels_file)
    assert_file_exists(labels_file)
    data_labels = read_fun(labels_file)
  }
  
  map(datalist, ~.apply_label_lookup(.x, data_labels))
}


#' @noRd
#' @keywords internal
#' @importFrom dplyr across all_of cur_column everything mutate pull select
.apply_label_lookup = function(data, data_labels, name_from="name", label_from="label"){
  name_from  = getOption("edc_col_name", default=name_from)
  label_from = getOption("edc_col_label", default=label_from)
  assert_class(data_labels, "data.frame")
  assert(name_from!=label_from, "Names and labels cannot have the same name in the lookup.")
  
  missing_col = setdiff(c(name_from, label_from), names(data_labels))
  if(length(missing_col)>0){
    cli_abort(c("The label lookup should contain columns {.val {missing_col}}.",
                i="It has columns {.val {names(data_labels)}}",
                i='Use {.code options(edc_col_name="aaa", edc_col_label="bbb")} to 
                override the default names.'),
              class="edc_label_missing_col")
  }
  
  label_vector = as.data.frame(data_labels) %>%
    select(name=all_of(name_from), label=all_of(label_from)) %>%
    pull(label, name=name)
  data %>% mutate(across(everything(), ~{
    label = unname(label_vector[cur_column()])
    attr(.x, "label") = label
    .x
  }))
}
