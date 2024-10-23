
  
#' Search for newer data
#' 
#' Search in some folders if a TrialMaster database more recent than the current extraction is present. By default, it will search the "data" folder and the OS usual "Downloads" folder. If a newer database is found, user will be asked if they want to move it to the "data" folder.
#'
#' @param archive TM archive path, giving the project name and date
#' @param source the path vector to be searched, default to both "data" and the usual "Downloads" folder
#' @param target the path where files should be copied
#' @param ask whether to ask the user to move the file to "data"
#' @param advice whether to advice how to move it instead, if `ask==FALSE`
#' @param ... unused
#'
#' @return the path to the newer file, invisibly.
#' @export
#' @importFrom cli cli_abort cli_code cli_inform
#' @importFrom dplyr setdiff
#' @importFrom fs dir_exists dir_ls file_copy path path_file path_home
#' @importFrom glue glue
#' @importFrom rlang check_dots_empty
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' 
#' @examples
#' \dontrun{
#'   archive = "data/MYPROJECT_ExportTemplate_xxx_SAS_XPORT_2024_06_01_12_00.zip"
#'   #tm = read_trialmaster(archive)
#'   search_for_newer_data(archive)
#' }
search_for_newer_data = function(archive, ..., 
                                 source=path_home("Downloads"), 
                                 target="data", 
                                 ask=TRUE, advice=TRUE){
  check_dots_empty()
  assert(length(target)==1)
  project_name = parse_file_projname(archive)
  project_date = parse_file_datetime(archive)
  
  if(!identical(source, target) && all(!dir_exists(setdiff(source, target)))){
    cli_abort(c("{.arg source} contains only nonexistent directories."))
  }
  source = unique(c(target, source))
  source = source[dir_exists(source)]

  files = dir_ls(source, type="file", fail=FALSE, 
                 regexp=glue(".*{project_name}.*\\.zip"))
  if(length(files)==0){
    cli_inform(c("No project files were found in {.arg source}."))
    return(invisible(FALSE))
  }
  
  files_dates = parse_file_datetime(files)
  max_date = max(files_dates, na.rm=TRUE)
  newest_file = NULL
  if(max_date > project_date){
    date_diff = as.numeric(round(max_date - project_date))
    newest_file = files[files_dates==max_date] %>% na.omit()
    newest_file_name = path_file(newest_file) %>% unique()
    if(length(newest_file_name)>1)  cli_abort("Several `newest_file_name`", .internal=TRUE)
    if(length(newest_file)>1) newest_file=newest_file[1]  #same file in several paths
    last_path = source[str_detect(newest_file, source)]
    already = if(last_path==target) "already " else ""
    cli_inform(c("!"="There is {already}a database in {.path {last_path}} that is {col_red(date_diff)} days 
                 more recent than the current extraction ({as.Date(max_date)} vs {as.Date(project_date)})."))
    
    from = newest_file
    to = path(target, newest_file_name)
    
    if(last_path==target){
      cli_inform(c("You might want to update your code to import the newer database.", 
                   i="Newer filename: {.val {newest_file_name}}"))
      return(invisible(newest_file))
    }
    
    user_input = 2
    if(is.numeric(ask)) user_input = ask #for tests
    else if(ask){
      user_input = cli_menu("Would you like to copy it to {.path {target}}?", choices=c("Yes", "No"))
    } 
    
    if(user_input==1){ #1=Yes
      file_copy(from, to)
      cli_inform(c("v"="New file copied, change your code to", "{.path {to}}"))
    } else if(isTRUE(advice)){
      # cli_inform(c("Run the following code to copy it to {.path target}:",
      #              " "='{.emph file.copy("\n\t{from}", \n"{to}", \ncopy.date=TRUE)}'))
      # cli_inform(c("Run the following code to copy it to {.path target}:",
      #              " "='{.emph file.copy(}',
      #              "  "='{.emph "{from}",}',
      #              "  "='{.emph "{to}", }',
      #              "  "='{.emph copy.date=TRUE}',
      #              " "='{.emph )}'
      # ))
      cli_inform("")
      cli_inform(c("Run the following code to copy it to {.arg target}:"))
      f = function(x) glue('  "{x}",')
      cli_code(
        'file.copy(',
        f(from),
        f(to),
        '  "copy.date=TRUE", ', 
        ')'
      )
    }
    
  }
  
  invisible(newest_file)
}
