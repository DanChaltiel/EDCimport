
#' Read the `.zip` archive of a TrialMaster export
#' 
#' Import the `.zip` archive of a TrialMaster trial export as a list of dataframes. Use `options(trialmaster_pw="xxx")` to indicate the password if the archive is protected. The archive filename should be leaved untouched as it contains the project name and the date of extraction. \cr
#' Generate a `.rds` cache file for future reads. \cr
#' If `7zip` is not installed or available, use [read_tm_all_xpt()] instead.
#'
#' @param archive the path to the archive
#' @param use_cache if `TRUE`, read the `.rds` cache if any or extract the archive and create a cache. If `FALSE` extract the archive without creating a cache file.
#' @param verbose one of `c(0, 1, 2)`. The higher, the more information will be printed
#'
#'
#' @inherit read_tm_all_xpt return
#' @export
read_trialmaster = function(archive, use_cache=TRUE, verbose=1){
  directory = dirname(archive)
  extract_datetime = parse_file_datetime(archive)
  proj_name = parse_file_project(archive)
  cache_file = glue("{directory}/{proj_name}_{format_ymd(extract_datetime)}.rds")
  if(file.exists(cache_file) && isTRUE(use_cache)){
    if(verbose>0) cli_inform("Reading cache: {.val {cache_file}}", class="read_tm_cache")
    rtn = readRDS(cache_file)
  } else {
    if(verbose>0) cli_inform("Unzipping {.val {archive}}", class="read_tm_zip")
    temp_folder = file.path2(tempdir(), str_remove(archive, "\\.zip"))
    dir.create(temp_folder, recursive=TRUE, showWarnings=FALSE)
    pw = getOption("trialmaster_pw")
    msg = extract_7z(archive, temp_folder, pw)
    if(verbose>1) cli_inform(msg)
    rtn = read_tm_all_xpt(temp_folder, extract_datetime)
    if(isTRUE(use_cache)) saveRDS(rtn, cache_file)
  }
  rtn
}

#' Read all `.xpt` files in a directory
#' 
#' Read all `.xpt` files in a directory (unzipped TrialMaster archive). \cr
#' If `7zip` is installed, you should probably rather use [read_trialmaster()] instead. \cr
#' If a `procformat.sas` file exists in the directory, formats will be applied.
#'
#' @param directory `<character>` unzipped archive using SAS_XPORT format. Will read the extraction date from the directory name
#' @param datetime_extraction the datetime of the data extraction
#'
#' @return a list containing one dataframe for each `.xpt` file in the folder, the extraction date (`datetime_extraction`), and a summary of all imported tables (`.tablelist`).
#' @export
#' @importFrom haven read_xpt
read_tm_all_xpt = function(directory, datetime_extraction){
  datasets = dir(directory, pattern = "\\.xpt$", full.names=TRUE)
  datasets_names = basename(datasets) %>% str_remove("\\.xpt")
  
  rtn = datasets %>% 
    set_names(tolower(datasets_names)) %>% 
    imap(~{
      read_xpt(.x) %>% 
        as_tibble() %>% 
        na_if("")
    })
  
  format_file = file.path2(directory, "procformat.sas")
  if(file.exists(format_file)){
    sas_formats = read_sas_format(format_file)
    rtn = rtn %>% 
      imap(~{
        .x %>% 
          apply_sas_formats(sas_formats) %>%
          haven::as_factor()
      })
  } else {
    cli_warn("No file {.val procformat.sas} found in {.arg directory}. Data formats cannot be applied.", 
             class="edc_tm_no_procformat")
  }
  
  date_extraction = format_ymd(datetime_extraction)
  rtn$date_extraction = date_extraction
  rtn$datetime_extraction = datetime_extraction
  rtn$.tablelist = tibble(dataset=tolower(datasets_names)) %>% 
    mutate(names=map(.data$dataset, ~names(rtn[[.x]])), 
           labels=map(.data$dataset, ~var_label(rtn[[.x]])))
  rtn
}


