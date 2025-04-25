
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
  JS=DT::JS;card=bslib::card;reactive=shiny::reactive;checkboxInput=shiny::checkboxInput;
  sliderInput=shiny::sliderInput;textOutput=shiny::textOutput;
  
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
    
    hidden_common_cols = reactive({
      if(is.null(input$hide_common) || input$hide_common==0) return(NULL)
      get_common_cols(lookup) %>% 
        filter(n_datasets/nrow(lookup) > input$hide_common/100) %>% 
        filter(!column %in% c(get_subjid_cols(lookup), get_crfname_cols(lookup))) %>% 
        pull(column)
    })
    
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
    
    #on Button Settings: show the Settings dialog
    observeEvent(input$btn_settings, {
      hide_filtered_default = input$hide_filtered %0% FALSE
      hide_common_default = input$hide_common %0% 0
      showModal(
        modalDialog(
          id="modal_settings",
          title = "Settings",
          card(
            checkboxInput("hide_filtered", "Hide empty datasets in the side panel", 
                          value=hide_filtered_default)
          ),
          card(
            sliderInput("hide_common", "Hide columns shared by this proportion of datasets (set to 0 to disable)", 
                        min=0, max=100, value=hide_common_default, post=" %"),
            textOutput("hide_common_result")
          ),
          easyClose = TRUE,
          footer = NULL
        )
      )
    })
    
    #on Button Search: show the Search dialog
    observeEvent(input$btn_search, {
      showModal(
        modalDialog(
          title = div(
            id="modal_search_header",
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
        result = edc_find_value(keyword, data=datasets, lookup=lookup)
        term = "value"
      } else {
        result = edc_find_column(keyword, lookup=lookup)
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
    
    #on Button Summary: show modal
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
    
    #output: hide common columns helper text
    output$hide_common_result = renderText({
      x = hidden_common_cols()
      if(is.null(x)) return("")
      glue("Columns hided: {paste(x, collapse=', ')}")
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
                          label=glue("Hide empty datasets in the side panel (N={n_filtered})"))
      
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
      
      hidden = input$hidden_hide %>% stringr::str_split_1("___")
      
      data = datasets[[dataset_selected()]] %>% 
        select(-any_of(hidden_common_cols()), -any_of(hidden)) %>% 
        relocate(any_of2(subjid_cols), .before=1) %>% 
        arrange(pick(any_of2(subjid_cols))) %>% 
        filter(if_any(any_of2(subjid_cols), 
                      ~all_selected | .x %in% subjid_selected))
      
      #ContextMenu: Fixed Column
      fixed = input$hidden_fixed %>% stringr::str_split_1("___")
      fixed = c(subjid_cols, fixed, input$hidden_group, input$hidden_color) %>% unique()
      fixed = fixed[nzchar(fixed)] %>% intersect(names(data))
      data = data %>% 
        relocate(any_of2(fixed), .before=1)
      
      #ContextMenu: Row Group
      i = which(names(data)==input$hidden_group)
      row_group = list(dataSrc = i)
      if(length(i)==0) row_group=NULL
      
      #ContextMenu: Row Color
      col_color = data[[input$hidden_color]]
      row_style = row_style_col = NULL
      if(!is.null(col_color)){
        lvl = levels(factor(col_color) %>% forcats::fct_na_value_to_level())
        pal = scales::viridis_pal(alpha=0.5)(length(lvl))
        row_style = styleEqual(levels = lvl, values=pal)
        row_style_col = input$hidden_color
      }
      
      data %>% 
        mutate_all(~str_remove_all(.x, "<.*?>")) %>% #remove HTML tage
        datatable(
          # rownames = FALSE,
          selection = "none",
          filter = "top",
          
          # height = "80%",
          # plugins = "ellipsis",
          extensions = c("FixedHeader", "FixedColumns", "ColReorder", "RowGroup", "KeyTable"),
          escape = FALSE, # Autorise HTML
          colnames = colnames_with_hover(data), # apply HTML names on hover
          options = lst(
            pageLength = 15,
            lengthMenu = list(c(15, 50, -1), 
                              c('15', '50', 'All')),
            fixedHeader = TRUE, #Not working as datatable not on page top
            # scrollY = "50%",
            # dom = "tp",
            # dom = 'Blfrtip',
            # autoWidth = TRUE,
            # scrollX = TRUE,
            keys = TRUE, #KeyTable: use keyboard to navigate
            rowGroup = row_group,
            colReorder = TRUE,
            columnDefs = list(hide_first(),
                              dt_ellipsis(data, n=10)),
              
            fixedColumns = list(leftColumns = length(fixed)),
          )
        ) %>% 
        formatStyle(
          columns = TRUE,
          `white-space` = "nowrap",
          `text-overflow` = "ellipsis",
          `overflow` = "hidden",
          # `max-width` = "150px",
          `height` = "20px"
        ) %>% 
        formatStyle(
          columns = TRUE,
          valueColumns=row_style_col,
          backgroundColor = row_style
        ) %>% 
        formatStyle(
          columns = fixed,
          backgroundColor = "white"
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
  if(!is.null(ids) && can_be_numeric(ids)){
    ids = as.numeric(ids) %>% unique() %>% sort()
  }
  ids
}


hide_first = function(){
  list(targets=0, visible=FALSE)
}

#' @importFrom glue glue
#' @importFrom purrr map_lgl
dt_ellipsis = function(data, n){
  list(list(
    targets = unname(which(map_lgl(data, ~is.character(.x)||is.factor(.x)))),
    render = DT::JS(
      "function(data, type, row, meta) {",
      "if(data==null || data==undefined) return ' ';",
      glue("return type === 'display' && data.length > {n} ?"),
      glue("'<span title=\"' + data + '\">' + data.substr(0, {n}) + '...</span>' : data;"),
      "}"
    )
  ))
}


#' Set the "label" dataset attribute on the "title" HTML attribute
#' Makes the column header show the column label on hover
#' @noRd
#' @importFrom purrr imap_chr
colnames_with_hover = function(data){
  data %>% 
    imap_chr(~{
      p_na = mean(is.na(.x)) %>% percent()
      label = attr(.x, "label") %0% ""
      if(label != "") label = paste0(label, "<br>")
      glue('<span class="data_column edc_label" title="{label}NA: {p_na}" data-bs-html="true">
           {.y}</span>')
    }) %>% 
    unname()
}