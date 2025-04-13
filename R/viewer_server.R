
#' @importFrom dplyr arrange filter if_any lst pick relocate select setdiff
#' @importFrom glue glue
#' @importFrom purrr map map_chr map_dbl
edc_viewer_server = function(datasets, lookup) {
  datatable=DT::datatable;formatStyle=DT::formatStyle;renderDT=DT::renderDT
  observe=shiny::observe;observeEvent=shiny::observeEvent;reactiveVal=shiny::reactiveVal;
  renderText=shiny::renderText;req=shiny::req;updateSelectInput=shiny::updateSelectInput
  dataTableProxy=DT::dataTableProxy;selectRows=DT::selectRows;styleRow=DT::styleRow
  icon=shiny::icon;showModal=shiny::showModal;modalDialog=shiny::modalDialog;
  plotOutput=shiny::plotOutput;renderPlot=shiny::renderPlot;
  updateCheckboxInput=shiny::updateCheckboxInput;textInput=shiny::textInput;
  actionButton=shiny::actionButton;renderUI=shiny::renderUI;
  showNotification=shiny::showNotification; uiOutput=shiny::uiOutput;
  layout_column_wrap=bslib::layout_column_wrap;styleEqual=DT::styleEqual;
  value_box=bslib::value_box;update_switch=bslib::update_switch;DTOutput=DT::DTOutput;
  
  .set_lookup(lookup, verbose=FALSE) #needed for bg launch
  subjid_cols = get_subjid_cols(lookup)
  
  project_name = attr(lookup, "project_name")
  if(is.null(project_name)) project_name="" 
  
  function(input, output, session) {
    dataset_selected = reactiveVal(NULL)
    
    ids = get_ids(datasets, subjid_cols)
    p1 = try(edc_crf_plot(datasets=datasets, lookup=lookup) + theme(legend.position="bottom"))
    p2 = try(edc_patient_gridplot(datasets=datasets, lookup=lookup) + labs(title=NULL, subtitle=NULL))
    if(inherits(p1, "try-error")) p1=ggplot()
    if(inherits(p2, "try-error")) p2=ggplot()
    
    #init
    selectRows(dataTableProxy("input_table"), selected=1)
    updateSelectInput(session, "subjid_selected", choices=c(ids))
    
    #On row selected: show datatable
    observeEvent(input$input_table_rows_selected, {
      selected = input$input_table_rows_selected
      dataset_selected(names(datasets[selected]))
    })
    
    #on Reset button: reset subjid choice
    observeEvent(input$reset_subjid, {
      updateSelectInput(session, "subjid_selected", choices=c(ids))
    })
    
    #on Search type change: update label
    observeEvent(input$search_type_value, {
      update_switch("search_type_value", label=ifelse(input$search_type_value, "for value", "for column"))
    })
    
    #on Search button: show the whole search dialog
    observeEvent(input$btn_search, {
      showModal(
        modalDialog(
          title = div(
            style = c("width: 100%; display: flex; justify-content: space-between;",
                      "gap: 50px; align-items: center;"),
            div(
              style = "display: flex; flex-grow: 1; align-items: center;",
              textInput("search_input", label=NULL, placeholder="Enter a keyword", width="100%")
            ),
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              div(
                style = "display: flex; align-items: center; height: 38px;", # Ajuste la hauteur
                bslib::input_switch("search_type_value", "for column", width = "175px")
              ),
              actionButton("search_validate", "Search", icon = icon("search"))
            )
          ),
          uiOutput("search_error"),
          DTOutput("search_result"),
          easyClose = TRUE,
          size = "xl",
          footer = NULL
        )
      )
    })
    
    #on Search validation: show results
    observeEvent(input$search_validate, {
      req(input$search_input)
      keyword = input$search_input
      if(is.null(keyword) || nchar(keyword)==0) {
        showNotification("Search input is empty")
        return(NULL)
      } 
      if(input$search_type_value){
        result = edc_find_value(keyword)
        term = "value"
      } else {
        result = edc_find_column(keyword)
        term = "column/label"
      }
      
      if(is.null(result) || nrow(result)==0) {
        output$search_error = renderUI(
          div(class="alert alert-danger", 
              glue('Could not find any {term} matching "{keyword}" in the database.'))
        )
        output$search_result = renderDT(NULL)
      } else {
        output$search_error = renderUI(NULL)
        output$search_result = renderDT({ 
          result %>% 
            select(-any_of("subjids")) %>% 
            mutate_all(~str_replace_all(format(.x), glue("(?i){keyword}"), 
                                        glue("<span style='color:red;'>\\0</span>"))) %>% 
            datatable(escape=FALSE)
        })
      }
      
      
    })
    
    #on Summary button: show modal
    observeEvent(input$btn_db_summary, {
      output$crf_plot = renderPlot(p1)
      output$patient_gridplot = renderPlot(p2)
      datetime_extraction = attr(lookup, "datetime_extraction")
      date_extraction = format(datetime_extraction, "%Y/%m/%d")
      delay_extraction = difftime(Sys.Date(), as.Date(datetime_extraction), units="days") %>% 
        as.numeric()
      vb1 = value_box(
        title = "Datasets",
        value = paste0(length(datasets), " datasets"),
        showcase = icon("database"),
      )
      vb2 = value_box(
        title = "Patients",
        value = paste0(max(lookup$n_id, na.rm=TRUE), " patients"),
        showcase = icon("person"),
      )
      vb3 = value_box(
        title = "Extraction",
        value = date_extraction,
        glue("{delay_extraction} days ago"),
        showcase = icon("calendar-days"),
      )
      showModal(
        modalDialog(
          title = paste0(project_name, " Database summary"),
          easyClose = TRUE,
          footer = NULL,
          size = "xl",
          layout_column_wrap(vb1, vb2, vb3, width=0),
          if(!is.null(p1)) plotOutput(session$ns("crf_plot")),
          plotOutput(session$ns("patient_gridplot")),
        )
      )
    })
    
    
    #output: datatable header text
    output$dataset_name = renderText({
      if(is.null(dataset_selected())) return("Loading")
      a = lookup %>% filter(dataset==dataset_selected())
      layout = ifelse(a$rows_per_id>1, glue("long ({a$rows_per_id} rows per patient)"), "wide")
      if(nrow(a)==0) return(glue("{name}: Corrupted dataset", name=dataset_selected()))
      glue("Dataset selected: `{a$dataset}` ({a$nrow} x {a$ncol}) - {a$n_id} patients - {layout}")
    })
    
    #output: sidebar data choice list
    output$input_table = renderDT({
      selected_subjid = input$subjid_selected
      all_subjid = unlist(lookup$subjids) %>% unique() %>% sort()
      if(is.null(selected_subjid)) selected_subjid = all_subjid
      
      lookup = lookup %>% 
        mutate(
          has_subjid = map_lgl(subjids, ~any(selected_subjid %in% .x)),
          is_error = !is.na(crfname) & crfname=="** Error in source file **",
          exclude = !has_subjid | is_error,
          row_color = case_when(!has_subjid~"red", is_error~"grey", .default=NA)
        )
      n_filtered = lookup %>% filter(exclude) %>% nrow()
      
      updateCheckboxInput(session, "hide_filtered", 
                          label=glue("Hide empty tables (N={n_filtered})"))
      
      if(isTRUE(input$hide_filtered) && length(selected_subjid)>0){
        lookup = lookup %>% filter(!exclude)
      }
      
      crf_name_i = which(names(lookup)=="crfname") - 1
      rtn = lookup %>% 
        as_tibble() %>% 
        datatable(
          rownames = FALSE,
          selection = "single",
          filter = "none",
          options = lst(
            pageLength = 500,
            dom = "t",
            columnDefs = list(list(visible=FALSE, targets=seq(3, ncol(lookup)-1))),
            rowCallback = JS(
              "function(row, data) {",
                glue("$('td', row).attr('title', data[{crf_name_i}]).addClass('edc_label');"),
              "}"
            ),
          )
        ) %>% 
        formatStyle(
          columns = TRUE,
          valueColumns="row_color",
          color = styleEqual(levels = c("red", "grey"), values = c("red", "grey")),
          `white-space` = "nowrap",
          `height` = "20px"
        )
      
      rtn
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
        mutate_all(~str_remove_all(.x, "<.*?>")) %>% #remove HTML tage
        datatable(
          # rownames = FALSE,
          selection = "none",
          filter = "top",
          # height = "80%",
          # plugins = "ellipsis",
          extensions = c("FixedHeader", "FixedColumns"),
          escape = FALSE, # Autorise HTML
          colnames = colnames_with_hover(data), # apply HTML names on hover
          options = lst(
            pageLength = 15,
            fixedHeader = TRUE,
            # scrollY = "50%",
            # dom = "tp",
            # autoWidth = TRUE,
            # scrollX = TRUE,
            columnDefs = dt_ellipsis(data, n=10), 
            fixedColumns = list(leftColumns = 2),
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


# Utils ---------------------------------------------------------------------------------------


#' Internal util to load functions without importFrom
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
      "return type === 'display' && data.length > {n} ?",
      "'<span title=\"' + data + '\">' + data.substr(0, {n}) + '...</span>' : data;",
      "}}"))
  ))
}


#' Set the "label" dataset attribute on the "title" HTML attribute
#' Makes the column header show the column label on hover
#' @noRd
colnames_with_hover = function(data){
  data %>% 
    imap_chr(~{
      p_na = mean(is.na(.x)) %>% percent()
      label = attr(.x, "label")
      if (!is.null(label) && label != "") label = paste0(label, "<br>")
      glue('<span class="edc_label" title="{label}NA: {p_na}" data-bs-html="true">
           {.y}</span>')
    }) %>% 
    unname()
}