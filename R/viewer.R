
#' Shiny data explorer
#' 
#' Run a Shiny application that allows to browse the datasets.
#' 
#' @param data A list of dataframes to view. If `NULL`, defaults to the last datasets loaded using EDCimport functions.
#' @param background Whether the app should run in a background process.
#' @param port The TCP port that the application should listen on. 
#'
#' @export
#' @importFrom rlang check_installed
#' @importFrom utils browseURL
edc_viewer = function(data=NULL, background=TRUE, port=1209){
  check_installed(c("DT", "bslib", "shiny"), "for `edc_viewer()` to work.")
  if(is.null(data)){
    lookup = edc_lookup(dataset)
    datasets = get_datasets(lookup)
  } else {
    datasets = data
    if(is.data.frame(data)) datasets = list(data) %>% set_names(caller_arg(data))
    if(!is_named(data)){
      cli_abort("Datasets in {.arg data} should have a name.", 
                class="edc_lookup_unnamed")
    }    
    lookup = build_lookup(datasets) %>% 
      extend_lookup() %>% 
      arrange(match(dataset, names(datasets)))
  }
  shiny_url = paste0("http://127.0.0.1:", port)
  
  
  if(isTRUE(background)){
    .run_background(datasets, lookup, port, shiny_url)
    return(edcimport_env$process)
  }
  
  browseURL(shiny_url)
  x=.launch_shiny(datasets, lookup, port)
  invisible(x)
}


#' @noRd
#' @keywords internal
.launch_shiny = function(datasets, lookup, port){
  app = shiny::shinyApp(edc_viewer_ui(datasets, lookup), 
                        edc_viewer_server(datasets, lookup))
  shiny::runApp(app, launch.browser=FALSE, port=port)
}

#' @noRd
#' @keywords internal
.run_background = function(datasets, lookup, port, shiny_url){
  check_installed("callr", "for `import_review()` to work in background")
  cur_port = paste0("port_", port)
  .check_current_port(cur_port)
  
  p = callr::r_bg(
    .launch_shiny, 
    args=list(datasets=datasets, lookup=lookup, port=port), 
    stdout=NULL, stderr="edc_viewer_errors.txt",
    package="EDCimport"
  )
  
  viewer_instance = lst(port, lookup, process=p) %>% 
    structure(class="edc_viewer_instance")
  edcimport_env$viewers[[cur_port]] = viewer_instance
  
  if(p$is_alive()) {
    pid = p$get_pid()
    cli_inform(c("Shiny app launched in the background (PID: {pid})",
                 i="Browse at {.url {shiny_url}}"))
    browseURL(shiny_url)
  } else {
    cli_inform(c(x="Error: Shiny app couldn't be launched", 
                 i="Is the port used in another session?"))
  }
  
}

#' @noRd
#' @keywords internal
.check_current_port = function(cur_port){
  cur_viewer = edcimport_env$viewers[[cur_port]]
  p = cur_viewer$process
  
  if(!is.null(cur_viewer) && p$is_alive()){
    cli_inform("An EDCviewer instance is already running on port {cur_viewer$port}:")
    print(cur_viewer)
    ok = askYesNo("Replace the running EDCviewer on this port?")
    if(!isTRUE(ok)){
      cli_abort("Operation cancelled by the user.", call=caller_call(n=2))
    }
    p$kill()
    p$wait(2000) #wait for the kill for 2s max
    if(p$is_alive()) p$kill_tree()
  }
}

#' @noRd
#' @keywords internal
.edc_viewer_kill = function(){
  edcimport_env$viewers %>% map(~.x$process$kill())
}

#' @noRd
#' @keywords internal
#' @export
print.edc_viewer_instance = function(x, ...){
  proj_name = glue(" for {nm}", nm=attr(x$lookup, "project_name")) %0% ""
  pid = x$process$get_pid()
  if(x$process$is_alive()){
    status = glue("running on port {x$port} (PID={pid})")
  } else {
    status = glue("not running (PID={pid})")
  }
  label = glue("EDCimport viewer{proj_name}, {status}")
  print(label)
}
