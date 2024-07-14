
  
#' Search for newer data
#' 
#' Search in some folders if a TrialMaster database more recent than the current extration is present. By default, it will search the "data" folder and the OS usual "Downloads" folder. If a newer database is found, user will be asked if they want to move it to the "data" folder.
#'
#' @param project_name the project name, treated as a regex
#' @param path the path vector to be searched, default to both "data" and the usual "Downloads" folder
#' @param ask whether to ask the user to move the file to "data"
#' @param advice whether to advice how to move it, if `ask==FALSE`
#'
#' @return the path to the newer file, invisibly.
#' @export
#' @importFrom cli cli_inform
#' @importFrom fs dir_ls file_copy path_file path_home
#' @importFrom glue glue
#' @importFrom stringr str_detect
search_for_newer_data = function(project_name, path=c("data", path_home("Downloads")), 
                               ask=TRUE, advice=NULL){
  files = dir_ls(path, type="file", regexp=glue(".*{project_name}.*\\.zip"))
  
  files_dates = parse_file_datetime(files)
  max_date = max(files_dates, na.rm=TRUE)
  
  if(max_date > datetime_extraction){
    date_diff = round(max_date - datetime_extraction)
    last_file = files[files_dates==max_date]
    last_file_short = path_file(last_file) %>% unique()
    if(length(last_file_short)>1)  rlang::abort("Several last_file_short", .internal=TRUE)
    if(length(last_file)>1) last_file=last_file[1]  #same file in several paths
    last_path = path[str_detect(last_file, path)]
      
    cli_inform("There is a database in {.path {last_path}} that is {date_diff} days more recent than the current extraction ({date_extraction}).")
    
    from=last_file
    to=paste0("data/", last_file_short)
    user_input = 2
    if(last_path=="data"){
      cli_inform("You might want to update your code to import the newer database.")
    } else if(ask){
      user_input = cli_menu("Would you like to copy it to {.path /data}?", choices=c("Yes", "No"))
      if(user_input==1){
        # ok=file.copy(from, to, copy.date=TRUE)
        ok=file_copy(from, to)
        if(!ok) stop("Error during copy")
      }
    } 
    if(isTRUE(advice) || is.null(advice) && user_input==2){
      cli_inform(c("Use the following code to copy it to {.path data}:",
                   " "='{.emph  file.copy("{from}", \n"{to}", \ncopy.date=TRUE)}'))
    }
    
  }
  
  invisible(last_file)
}
