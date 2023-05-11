
#' Read the `.zip` archive of a TrialMaster export
#' 
#' Import the `.zip` archive of a TrialMaster trial export as a list of dataframes. The archive filename should be leaved untouched as it contains the project name and the date of extraction. \cr
#' Generate a `.rds` cache file for future reads. \cr
#' If `7zip` is not installed or available, use [read_tm_all_xpt()] instead.
#'
#' @param archive the path to the archive
#' @param use_cache if `TRUE`, read the `.rds` cache if any or extract the archive and create a cache. If `FALSE` extract the archive without creating a cache file.
#' @param pw The password if the archive is protected. To avoid writing passwords in plain text, it is better to indicate your password using `options(trialmaster_pw="xxx")` in another file than using `pw="xxx"`.
#' @param clean_names_fun a function to clean column names, e.g. [janitor::clean_names()]
#' @param split_mixed whether to split mixed datasets. See [split_mixed_datasets].
#' @param verbose one of `c(0, 1, 2)`. The higher, the more information will be printed'
#' @param ... unused
#'
#' @inherit read_tm_all_xpt return
#' @export
read_trialmaster = function(archive, ..., use_cache=TRUE, 
                            clean_names_fun=NULL,
                            split_mixed=FALSE,
                            pw=getOption("trialmaster_pw"), 
                            verbose=getOption("edc_verbose", 1)){
  directory = dirname(archive)
  extract_datetime = parse_file_datetime(archive)
  check_dots_empty()
  if(!file.exists(archive)){
    cli_abort("Archive {.val {archive}} does not exist.", 
             class="edc_tm_404")
  }
  if(is.na(extract_datetime)){
    cli_warn(c("Extraction datetime could not be read from archive's name.", 
               x="Archive's name should contain the datetime as {.code SAS_XPORT_yyyy_mm_dd_hh_MM}", 
               i="Actual archive's name: {.val {archive}}"), 
             class="edc_tm_bad_name")
  }
  
  cache_file = glue("{directory}/trialmaster_export_{format_ymdhm(extract_datetime)}.rds")
  if(file.exists(cache_file) && isTRUE(use_cache)){
    if(verbose>0) cli_inform("Reading cache: {.file {cache_file}}", class="read_tm_cache")
    rtn = readRDS(cache_file)
    if(is.null(getOption("edc_lookup", NULL))){
      options(edc_lookup=rtn$.lookup)
    }
  } else {
    if(verbose>0) cli_inform("Unzipping {.file {archive}}", class="read_tm_zip")
    temp_folder = file.path2(tempdir(), str_remove(basename(archive), "\\.zip"))
    dir.create(temp_folder, recursive=TRUE, showWarnings=FALSE)
    msg = extract_7z(archive, temp_folder, pw)
    if(verbose>1) cli_inform(msg)
    if(is.na(extract_datetime)) extract_datetime = get_folder_datetime(temp_folder)
    format_file = file.path2(temp_folder, "procformat.sas")
    if(!file.exists(format_file)){
      cli_warn("No file {.val procformat.sas} found in {.arg directory}. 
             Data formats cannot be applied.", 
             class="edc_tm_no_procformat_warning") 
      format_file = NULL
    }
    rtn = read_tm_all_xpt(temp_folder, format_file=format_file, 
                          clean_names_fun=clean_names_fun, 
                          split_mixed=split_mixed,
                          datetime_extraction=extract_datetime)
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
#' @param directory the path to the unzipped archive using SAS_XPORT format. Will read the extraction date from the directory name.
#' @param format_file the path to the `procformat.sas` file that should be used to apply formats. Use `NULL` to not apply formats.
#' @param datetime_extraction the datetime of the data extraction. Default to the most common date of last modification in `directory`.
#'
#' @return a list containing one dataframe for each `.xpt` file in the folder, the extraction date (`datetime_extraction`), and a summary of all imported tables (`.lookup`). If not set yet, option `edc_lookup` is automatically set to `.lookup`.
#' @export
#' @importFrom haven read_xpt
read_tm_all_xpt = function(directory, ..., format_file="procformat.sas", 
                           clean_names_fun=NULL, split_mixed=FALSE, 
                           datetime_extraction=NULL){
  check_dots_empty()
  clean_names_fun=get_clean_names_fun(clean_names_fun)
  datasets = dir(directory, pattern = "\\.xpt$", full.names=TRUE)
  datasets_names = basename(datasets) %>% str_remove("\\.xpt")
  if(is.null(datetime_extraction)) datetime_extraction=get_folder_datetime(directory)
  
  rtn = datasets %>% 
    set_names(tolower(datasets_names)) %>% 
    imap(~tryCatch(read_xpt(.x), error=function(e) e))
  
  if(!is.null(format_file)){
    procformat = file.path2(directory, format_file)
    if(!file.exists(procformat)) procformat = format_file
    if(!file.exists(procformat)) {
      cli_abort("File {.file {format_file}} does not exist.", 
                class="edc_tm_no_procformat_error")
    }
    sas_formats = read_sas_format(procformat)
    rtn = rtn %>% 
      imap(~{
        if(is_error(.x)) return(.x)
        .x %>% 
          as_tibble() %>% 
          mutate(across(where(is.character), na_if, y="")) %>% 
          apply_sas_formats(sas_formats) %>%
          clean_names_fun() %>% 
          haven::as_factor()
      })
  }
  
  if(split_mixed){
    mixed = split_mixed_datasets(rtn)
  }
  browser()
  
  errs = keep(rtn, is_error)
  if(length(errs)>0){
    cli_warn(c("SAS dataset{?s} {.val {names(errs)}} could not be read from 
               the archive using {.fun haven::read_xpt}.",
               i="You can print the object{?s} to see the error message (e.g. 
               run {.run print({names(errs[1])})})."), 
             class="edc_tm_problem_warning")
  }
  
  
  
  rtn$date_extraction = format_ymd(datetime_extraction)
  rtn$datetime_extraction = datetime_extraction
  rtn$.lookup = get_lookup(rtn)
  if(is.null(getOption("edc_lookup", NULL))){
    options(edc_lookup=rtn$.lookup)
  }
  rtn
}

