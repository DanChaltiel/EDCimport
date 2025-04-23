
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
  
  launch_shiny = function(datasets, lookup, port){
    # devtools::load_all(helpers=FALSE)
    
    app = shiny::shinyApp(edc_viewer_ui(datasets, lookup), 
                          edc_viewer_server(datasets, lookup))
    shiny::runApp(app, launch.browser=FALSE, port=port)
  }
  
  if(isTRUE(background)){
    check_installed("callr", "for `import_review()` to work in background")
    brw = Sys.getenv("R_BROWSER")
    
    if(exists("process", envir=edcimport_env)) {
      cli_inform("Killing previous background process (PID: {edcimport_env$process$get_pid()})")
      edcimport_env$process$kill()
    }
    
    edcimport_env$process = callr::r_bg(
      launch_shiny, 
      args=list(datasets=datasets, lookup=lookup, port=port), 
      stdout="out", stderr="errors",
      package="EDCimport"
    )
    if(edcimport_env$process$is_alive()) {
      cli_inform(c("Shiny app launched in the background (PID: {edcimport_env$process$get_pid()})",
                   i="Browse at {.url {shiny_url}}"))
      browseURL(shiny_url)
    } else {
      cli_inform(c(x="Error: Shiny app couldn't be launched"))
    }
    return(edcimport_env$process)
  }
  
  browseURL(shiny_url)
  x=launch_shiny(datasets, lookup, port)
  invisible(x)
}


