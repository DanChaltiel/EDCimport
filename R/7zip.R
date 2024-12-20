
#' Retrieve the path to 7zip executable directory
#' 
#' Retrieve the path to 7zip executable directory. Use `options(path_7zip="path/to/7zip")` to change its behavior.
#'
#' @return the path to 7zip executable directory
#' @importFrom cli cli_abort
#' @importFrom fs dir_exists
#' 
#' @noRd
#' @keywords internal
get_7z_dir = function(){
  #TODO multiple default depending on OS?
  default = "C:/Program Files/7-Zip/"
  path_7zip = getOption("path_7zip", default)
  #TODO dir_exists is weak af, better check the executable. But what about linux and macos?
  if(!dir_exists(path_7zip)){
    cli_abort(c("Path {.val {path_7zip}} does not lead to 7-Zip.", 
                x="Try to add 7-Zip to the PATH environment variable.", 
                x='Otherwise, use {.code options(path_7zip="path/to/7zip")} to change the', 
                i='See section PATH in {.code ?extract_7z} for more informations.'))
  }
  path_7zip
}



#' Extract an archive using 7zip
#' 
#' Use 7zip to extract an archive. Obviously, 7zip should be installed beforehand.
#'
#' @param archive the archive file
#' @param target_dir the target directory
#' @param password the password of the archive, if any
#' @param path_7zip See section below. Default to [get_7z_dir()].
#' 
#' @section Install 7-zip:
#' 
#' If 7-zip is not installed on your computer, the easiest way to get it is to run [installr::install.7zip()].
#' 
#' @section Add 7zip to the `PATH`:
#' 
#' For this function to work, 7-zip should be registered as a `PATH` environment variable. There are many resources online to help you register if, e.g. https://www.java.com/en/download/help/path.html. 
#' If you cannot change the `PATH` on your computer, then you can add the path to 7zip executable directory in the parameter `path_7zip` or globally using `options(path_7zip="path/to/7zip")`. 
#' This will add 7zip to the `PATH` for the current session.
#'
#' @return the success/error message. Mainly used for its side effect of extracting the archive.
#' @seealso https://info.nrao.edu/computing/guide/file-access-and-archiving/7zip/7z-7za-command-line-guide#section-17
#' @importFrom cli cli_abort cli_warn
#' @importFrom fs dir_create dir_exists file_exists
#' @importFrom glue glue
#' @importFrom stringr str_detect
#' @noRd
#' @keywords internal
extract_7z = function(archive, target_dir, password=NULL, preserve_dirs=TRUE, path_7zip=NULL){
  if(!file_exists(archive)){
    cli_abort("Archive file {.val {archive}} does not exist.")
  }
  if(!dir_exists(target_dir)){ 
    dir_create(target_dir)
  }
  cur_path = Sys.getenv("PATH")
  if(!str_detect(cur_path, "7-Zip")){
    if(is.null(path_7zip)) path_7zip = get_7z_dir()
    Sys.setenv(PATH=paste0(cur_path, ";", path_7zip))
  }
  
  read_arg = if(isTRUE(preserve_dirs)) "x" else "e"
  pwc = if(is.null(password)) "" else glue('-p"{password}"')
  cmd = glue('7z {read_arg} -o"{target_dir}" "{archive}" {pwc} -aoa')
  msg = try(system(cmd, intern=TRUE), 
            silent=TRUE) %>% 
    suppressWarnings()
  if(inherits(msg, "try-error")){
    cli_abort(msg, class="edc_7z_cmd_error")
  }
  
  status = attr(msg, "status")
  if(!nzchar(msg[1])) msg=msg[-1]
  msg = paste(msg, sep="\n")
  # attr(msg, "status") = recode(attr(msg, "status"),
  #                              "0"="No error",
  #                              "1"="Warning",
  #                              "2"="Fatal error",
  #                              "7"="Command line error",
  #                              "8"="Not enough memory for operation",
  #                              "255"="User stopped the process")
  if(!is.null(status) && status!=0){
    if(status==1){
      cli_warn(msg, class="edc_7z_warn")
    } else {
      if(any(str_detect(tolower(msg), "password"))){
        cli_abort(c("Wrong password, archive could not be extracted.",
                    i="Did you forget the {.code read_trialmaster(pw=xxx)} 
                    argument or the option {.code options(trialmaster_pw=xxx)}?"), 
                  class="edc_7z_bad_password_error")
      }
      cli_abort(msg, class="edc_7z_error")
    }
  }
  
  msg
}
