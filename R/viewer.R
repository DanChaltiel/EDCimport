#TODO: Afficher en rouge les datasets où SUBJID est absent
#TODO: height en % plutôt qu'en px?
#TODO: layout plutôt que dom (mettre tous les widgets en haut si possible)

edc_viewer_ui = function(){
  datasets = get_datasets()
  datasets = datasets[rev(order(map_dbl(datasets, nrow)))]
  
  lookup = edc_lookup()
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
    window_title=paste(project_name, " -EDCimport"),
    title=title,
    height="100vh",
    sidebar = sidebar(
      card(
        card_title("Subjects:", actionButton("reset_subjid", "Reset", style="padding:5px")),
        selectInput("subjid_selected", label=NULL, choices="All", selected="All",
                    multiple=TRUE),
      ),
      card(
        # fill=FALSE,
        # height="60vh",
        card_title("Select a dataset:"),
        tags$ul(
          style = "list-style: none; padding: 0;overflow-y: auto; ",
          lapply(names(datasets), function(name) {
            tags$li(
              style = "margin-bottom: 8px;",
              actionLink(
                inputId = paste0("link_", name),
                label = name,
                style = "display: block; padding: 8px; border: 1px solid #007BFF;
                  border-radius: 4px; text-align: center; text-decoration: none;
                  color: #007BFF; font-weight: bold; background-color: #ffffff;
                  transition: background-color 0.2s;"
              )
            )
          })
        )
      )
    ),
    card(
      card_header(
        textOutput("dataset_name"),
      ),
      card_body(
        DTOutput("table")
      )
    )
  )
}

# Serveur
edc_viewer_server = function(input, output, session) {
  datasets = get_datasets()
  datasets = datasets[rev(order(map_dbl(datasets, nrow)))]
  dataset_selected = reactiveVal(NULL)
  subjid_cols = get_subjid_cols()
  ids = datasets %>% 
    map(~select(.x, any_of2(subjid_cols))) %>% 
    unlist() %>% unique() %>% sort()
  if(can_be_numeric(ids)){
    ids = as.numeric(ids) %>% unique() %>% sort()
  }
  
  dataset_selected(names(datasets)[1]) #init: show first dataset
  
  observe({
    lapply(names(datasets), function(name) {
      observeEvent(input[[paste0("link_", name)]], {
        dataset_selected(name)
      })
    })
  })
  
  observe({
    updateSelectInput(session, "subjid_selected", choices=c("All", ids), 
                      selected="All")
  })
  
  observeEvent(input$reset_subjid, {
    updateSelectInput(session, "subjid_selected", choices=c("All", ids), 
                      selected="All")
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
  
  output$dataset_name = renderText({
    # req(dataset_selected())
    data = datasets[[dataset_selected()]]
    glue("Dataset selected: {name} ({nrow(data)} x {ncol(data)})", name=dataset_selected())
  })
  
  output$table = renderDT({
    req(dataset_selected())
    subjid_selected=input$subjid_selected
    if(is.null(subjid_selected)) subjid_selected = "All"
    if(length(subjid_selected)>1){
      updateSelectInput(session, "subjid_selected", 
                        selected=setdiff(subjid_selected, "All"))
    }
    # browser()
    data = datasets[[dataset_selected()]] %>% 
      relocate(any_of2(subjid_cols), .before=1) %>% 
      arrange(pick(any_of2(subjid_cols))) %>% 
      filter(if_any(any_of2(subjid_cols), 
                    ~any(subjid_selected=="All")|.x %in% subjid_selected))
    labels = map_chr(data, ~attr(.x, "label"))
    
    # Génération des noms de colonnes avec attribut "title" pour le hover
    colnames_with_hover =  map_chr(colnames(data), ~{
      label = attr(data[[.x]], "label")
      if (!is.null(label) && label != "") {
        # sprintf('<span title="%s">%s</span>', label, .x)
        glue('<span title="{label}">{.x}</span>')
      } else {
        .x
      }
    })
    
    datatable(
      data,
      # rownames = FALSE,
      selection = "none",
      filter = "top",
      # height = "80%",
      # plugins = "ellipsis",
      escape = FALSE, # Autorise HTML
      colnames = colnames_with_hover, # Applique les noms HTML
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
      # Applique le CSS pour limiter la hauteur des cellules
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

dt_ellipsis = function(data, n){
  list(list(
    targets = unname(which(map_lgl(data, ~is.character(.x)||is.factor(.x)))),
    render = JS(glue(
      "function(data, type, row, meta) {{",
      "if(data==null || data==undefined) return ' ';",
      "console.log(data);",
      "return type === 'display' && data.length > {n} ?",
      "'<span title=\"' + data + '\">' + data.substr(0, {n}) + '...</span>' : data;",
      "}}"))
  ))
}

#' Title
#'
#' @returns
#' @export
edc_viewer = function(background=FALSE){
  check_installed("bslib")
  check_installed("shiny")
  check_installed("DT")
  library(shiny)
  # library(shinyWidgets)
  library(bslib)
  # library(shinyjs)
  library(DT)
  launch_shiny = function(){
    app = shiny::shinyApp(EDCimport:::edc_viewer_ui, EDCimport:::edc_viewer_server)
    shiny::runApp(app, launch.browser=FALSE, port=1231)
  }
  
  if(isTRUE(background)){
    check_installed("callr", "for `import_review()` to work in background")
    brw = Sys.getenv("R_BROWSER")
    # x=callr::r_bg(launch_shiny, stdout="out", stderr="errors",
    #               package="EDCimport", env = c(R_BROWSER=brw))
    process=callr::r_bg(launch_shiny, package="EDCimport")
    if (process$is_alive()) {
      message("Application lancée en arrière-plan.")
      browseURL("http://127.0.0.1:1231")
    } else {
      message("Échec du lancement de l'application.")
    }
    return(process)
  }
  
  launch_shiny()
  invisible()
}

# devtools::load_all(".");x=edc_viewer();x$read_error()



