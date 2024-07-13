

#' Create a clinical research project
#' 
#' Create a standardized, empty clinical research project.
#'
#' @param path A path. If it does not exist, it is created.
#' @param open If `TRUE`, opens the new project in RStudio.
#'
#' @return Path to the project, invisibly.
#' @export
edc_new_project = function(path, open=TRUE){
  check_installed("usethis", "for `init_project()` to work.")
  dir_create(path)
  if(!is_dir(path)){
    cli_abort("`path` should be a directory.")
  }
  path_nchilds = length(dir_ls(path))
  if(path_nchilds>0){
    cli_abort("`path` should be empty, but has {path_nchilds} child{?s}.")
  }
  
  proj_name = basename(path)
  stat_name = Sys.getenv("USERNAME", unset="Myself")
  rproj_file = paste0(proj_name, ".Rproj")
  
  #copy template files from package to path
  pkg_files = dir_ls(templ_dir, type="file", recurse=TRUE)
  new_files = pkg_files %>% 
    str_replace(fixed(as.character(templ_dir)), path) %>% 
    str_replace("xxxxxx.Rproj", rproj_file) %>% 
    path()
  dir_create(path_dir(new_files))
  file_copy(pkg_files, new_path=new_files, overwrite=TRUE)
  
  #replace template variables
  new_files %>% 
    walk(~{
      .x %>% 
        file_str_replace("VAR_PROJ_NAME"=proj_name, 
                         "VAR_STAT_NAME"=stat_name,
                         "VAR_RPROJ_FILE"=rproj_file,
                         "VAR_DATE"=today_ymd())
    })
  
  #adding box headers?
  # header_data = c("Project: {proj_name}", 
  #                 "Statistician: {stat_name}") %>% 
  #   map_chr(glue, .envir=current_env())
  # 
  # header = cli::boxx(header_data, width=80) %>% paste0("# ", .)
  
  #open in RStudio
  if(isTRUE(open) && is_installed("rstudioapi")) {
    if(rstudioapi::isAvailable() && rstudioapi::hasFun("openProject")) {
      cli_inform(c("v"="Opening {.path {path}} in new RStudio session"))
      rstudioapi::openProject(path, newSession = TRUE)
      invisible(FALSE)
    }
  }
  
  invisible(path)
}


# Utils ---------------------------------------------------------------------------------------

#' @noRd
#' @keywords internal
#' @param ... names=pattern, values=replacement
file_str_replace = function(file, ...) {
  readLines(file) %>% 
    str_replace_all(c(...)) %>% 
    writeLines(con=file)
  file
}
