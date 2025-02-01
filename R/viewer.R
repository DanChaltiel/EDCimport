

#' @importFrom cli format_inline
#' @importFrom purrr map_dbl
edc_viewer_ui = function(datasets, lookup){
  card=bslib::card;card_body=bslib::card_body;card_header=bslib::card_header;
  card_title=bslib::card_title;page_sidebar=bslib::page_sidebar;sidebar=bslib::sidebar
  DTOutput=DT::DTOutput
  actionButton=shiny::actionButton;selectInput=shiny::selectInput;actionLink=shiny::actionLink;
  tags=shiny::tags;textOutput=shiny::textOutput
  
  extraction = attr(lookup, "datetime_extraction")
  EDCimport_version = attr(lookup, "EDCimport_version")
  project_name = attr(lookup, "project_name")
  par_extraction = par_version = par_projname = ""
  if(!is.null(project_name)) 
    par_projname = format_inline("- {project_name} ")
  if(!is.null(extraction)) 
    par_extraction = format_inline("(extraction of {format_ymd(extraction)}) ")
  if(!is.null(EDCimport_version)) 
    par_version = format_inline("- EDCimport v{EDCimport_version}")
  title = format_inline("{{EDCimport}} Data browsing {par_projname}{par_extraction}{par_version}")
  
  page_sidebar(
    window_title=paste(project_name, " - EDCimport"),
    title=title,
    height="100vh",
    sidebar = sidebar(
      width = 350,
      card(
        card_title("Subjects:", actionButton("reset_subjid", "Reset", style="padding:5px")),
        selectInput("subjid_selected", label=NULL, choices=1, multiple=TRUE),
      ),
      card(
        card_title("Select a dataset:", container = htmltools::h3),
        DTOutput("input_table", fill = FALSE),
      )
    ),
    card(
      card_header(
        textOutput("dataset_name"),
      ),
      card_body(
        DTOutput("table")
      )
    ),
    tags$head(tags$style('.card{overflow: visible !important;}'),
              tags$style('.card-body{overflow: visible !important;}'))
  )
}


#' @importFrom dplyr arrange filter if_any lst pick relocate select setdiff
#' @importFrom glue glue
#' @importFrom purrr map map_chr map_dbl
edc_viewer_server = function(datasets, lookup) {
  datatable=DT::datatable;formatStyle=DT::formatStyle;renderDT=DT::renderDT
  observe=shiny::observe;observeEvent=shiny::observeEvent;reactiveVal=shiny::reactiveVal;
  renderText=shiny::renderText;req=shiny::req;updateSelectInput=shiny::updateSelectInput
  
  .set_lookup(lookup, verbose=FALSE)
  subjid_cols = get_subjid_cols(lookup)
  datasets = datasets
  
  function(input, output, session) {
    dataset_selected = reactiveVal(NULL)
    
    ids = get_ids(datasets, subjid_cols)
    
    #init
    selectRows(dataTableProxy("input_table"), selected=1)
    updateSelectInput(session, "subjid_selected", choices=c(ids))
    
    #show datatable on row selected
    observeEvent(input$input_table_rows_selected, {
      selected = input$input_table_rows_selected
      dataset_selected(names(datasets[selected]))
    })
    
    #reset subjid choice on click on Reset button
    observeEvent(input$reset_subjid, {
      updateSelectInput(session, "subjid_selected", choices=c(ids))
    })
    
    
    #output: datatable header
    output$dataset_name = renderText({
      if(is.null(dataset_selected())) return("Loading")
      data = datasets[[dataset_selected()]]
      if(is.null(data)) return(glue("{name}: Corrupted dataset", name=dataset_selected()))
      glue("Dataset selected: {name} ({nrow(data)} x {ncol(data)})", name=dataset_selected())
    })
    
    #output: datatable choice list
    output$input_table = renderDT({
      selected_subjid = input$subjid_selected
      red_rows = map_lgl(lookup$subjids, ~!any(selected_subjid %in% .x)) %>% which()
      grey_rows = which(lookup$crfname=="** Error in source file **")
      if(length(selected_subjid)==0) red_rows = 0
      if(length(red_rows)==0) red_rows = 0
      if(length(grey_rows)==0) grey_rows = 0
      
      lookup %>% 
        select(dataset, nrow, ncol) %>% 
        as_tibble() %>% 
        datatable(
          rownames = FALSE,
          selection = "single",
          filter = "none",
          options = lst(
            pageLength = 500,
            dom = "t"
          )
        ) %>% 
        formatStyle(
          columns = 1:3,
          color = styleRow(red_rows, "red"),
          `white-space` = "nowrap",
          `height` = "20px"
        ) %>% 
        formatStyle(
          columns = 1:3,
          color = styleRow(grey_rows, "grey")
        )
    })
    
    #output: datatable body
    output$table = renderDT({
      req(dataset_selected())
      subjid_selected = input$subjid_selected
      all_selected = length(subjid_selected)==0
      if(is.null(datasets[[dataset_selected()]])) return(tibble())
      data = datasets[[dataset_selected()]] %>% 
        relocate(any_of2(subjid_cols), .before=1) %>% 
        arrange(pick(any_of2(subjid_cols))) %>% 
        filter(if_any(any_of2(subjid_cols), 
                      ~all_selected | .x %in% subjid_selected))
      labels = map_chr(data, ~attr(.x, "label") %0% NA)
      
      data %>% 
        datatable(
          # rownames = FALSE,
          selection = "none",
          filter = "top",
          # height = "80%",
          # plugins = "ellipsis",
          escape = FALSE, # Autorise HTML
          colnames = colnames_with_hover(data), # Applique les noms HTML
          options = lst(
            pageLength = 15,
            # dom = "tp",
            # autoWidth = TRUE,
            # scrollX = TRUE,
            columnDefs = dt_ellipsis(data, n=10),
            # columnDefs = list(list(
            #   targets = unname(which(map_lgl(data, ~is.character(.x)||is.factor(.x)))),
            #   render = JS("$.fn.dataTable.render.ellipsis(17, false )")
            # ))
          )
        ) %>% 
        formatStyle(
          columns = seq_along(colnames(data)),
          `white-space` = "nowrap",
          `text-overflow` = "ellipsis",
          `overflow` = "hidden",
          # `max-width` = "150px",
          `height` = "20px"
        )
    })
  }
}


#' Shiny data explorer
#' 
#' Run a Shiny application that allows to browse the datasets.
#' 
#' @param background should the app run in a background process
#'
#' @export
#' @importFrom rlang check_installed
#' @importFrom utils browseURL
edc_viewer = function(background=TRUE){
  lookup = edc_lookup(dataset)
  datasets = get_datasets(lookup)
  
  launch_shiny = function(datasets, lookup){
    # devtools::load_all(helpers=FALSE)
    
    app = shiny::shinyApp(EDCimport:::edc_viewer_ui(datasets, lookup), 
                          EDCimport:::edc_viewer_server(datasets, lookup))
    shiny::runApp(app, launch.browser=FALSE, port=1231)
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
      args=list(datasets=datasets, lookup=lookup), 
      stdout="out", stderr="errors",
      # env = c(R_BROWSER=brw),
      # error="error",
      package="EDCimport"
    )
    if(edcimport_env$process$is_alive()) {
      cli_inform(c("Shiny app launched in the background (PID: {edcimport_env$process$get_pid()})",
                   i="Browse at {.url http://127.0.0.1:1231}"))
      browseURL("http://127.0.0.1:1231")
    } else {
      cli_inform(c(x="Error: Shiny app couldn't be launched"))
    }
    return(edcimport_env$process)
  }
  
  browseURL("http://127.0.0.1:1231")
  x=launch_shiny(datasets, lookup)
  # print(x)
  invisible(x)
}

# devtools::load_all(".");x=edc_viewer(TRUE);
# cat(x$read_error())



# Utils ---------------------------------------------------------------------------------------


#' @noRd
#' @examples
#' import_to_list("#' @importFrom bslib card card_body sidebar")
#' import_to_list("shiny actionButton selectInput actionLink tags textOutput")
import_to_list = function(x){
  x = str_remove(x, "#' @importFrom ") %>% stringr::str_split_1(" ")
  paste0(x[-1], "=", x[1], "::", x[-1]) %>% paste(collapse=";")
}


get_ids = function(datasets, subjid_cols){
  ids = datasets %>%
    keep(is.data.frame) %>% 
    map(~select(.x, any_of2(subjid_cols))) %>% 
    unlist() %>% unique() %>% sort()
  if(can_be_numeric(ids)){
    ids = as.numeric(ids) %>% unique() %>% sort()
  }
  ids
}


#' @importFrom glue glue
#' @importFrom purrr map_lgl
dt_ellipsis = function(data, n){
  list(list(
    targets = unname(which(map_lgl(data, ~is.character(.x)||is.factor(.x)))),
    render = DT::JS(glue(
      "function(data, type, row, meta) {{",
      "if(data==null || data==undefined) return ' ';",
      "console.log(data);",
      "return type === 'display' && data.length > {n} ?",
      "'<span title=\"' + data + '\">' + data.substr(0, {n}) + '...</span>' : data;",
      "}}"))
  ))
}


#' Set the "label" dataset attribute on the "title" HTML attribute
#' Makes the column header show the column label on hover
#' @noRd
colnames_with_hover = function(data){
  map_chr(colnames(data), ~{
    label = attr(data[[.x]], "label")
    if (!is.null(label) && label != "") {
      glue('<span title="{label}">{.x}</span>')
    } else {
      .x
    }
  })
}