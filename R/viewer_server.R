
#' @importFrom dplyr any_of arrange as_tibble case_when filter if_any lst mutate mutate_all pick pull relocate select
#' @importFrom ggplot2 ggplot labs theme
#' @importFrom glue glue
#' @importFrom purrr map_dbl map_lgl
#' @importFrom stringr str_remove_all str_replace_all
#' @importFrom tibble tibble
edc_viewer_server = function(datasets, lookup) {
  datatable=DT::datatable;formatStyle=DT::formatStyle;renderDT=DT::renderDT
  observe=shiny::observe;observeEvent=shiny::observeEvent;reactiveVal=shiny::reactiveVal;
  renderText=shiny::renderText;req=shiny::req;updateSelectInput=shiny::updateSelectInput
  dataTableProxy=DT::dataTableProxy;selectRows=DT::selectRows;styleRow=DT::styleRow
  icon=shiny::icon;showModal=shiny::showModal;modalDialog=shiny::modalDialog;
  plotOutput=shiny::plotOutput;renderPlot=shiny::renderPlot;
  updateCheckboxInput=shiny::updateCheckboxInput;textInput=shiny::textInput;
  actionButton=shiny::actionButton;renderUI=shiny::renderUI;div=shiny::div;
  showNotification=shiny::showNotification; uiOutput=shiny::uiOutput;
  layout_column_wrap=bslib::layout_column_wrap;styleEqual=DT::styleEqual;
  value_box=bslib::value_box;update_switch=bslib::update_switch;DTOutput=DT::DTOutput;
  JS=DT::JS;card=bslib::card;reactive=shiny::reactive;checkboxInput=shiny::checkboxInput;
  sliderInput=shiny::sliderInput;textOutput=shiny::textOutput;
  
  .set_lookup(lookup, verbose=FALSE) #needed for bg launch
  subjid_cols = get_subjid_cols(lookup)
  project_name = attr(lookup, "project_name") %0% ""
  
  function(input, output, session) {
    dataset_selected = reactiveVal(NULL)
    
    ids = get_ids(datasets, subjid_cols)
    p1 = try(edc_crf_plot(datasets=datasets, lookup=lookup) + theme(legend.position="bottom"))
    p2 = try(edc_patient_gridplot(datasets=datasets, lookup=lookup) + labs(title=NULL, subtitle=NULL))
    if(inherits(p1, "try-error")) p1=ggplot()
    if(inherits(p2, "try-error")) p2=ggplot()
    
    #init
    session$onFlushed(once = TRUE, function() {
      selectRows(dataTableProxy("input_table"), selected=1)
      updateSelectInput(session, "subjid_selected", choices=c(ids))
    })
    
    hidden_common_cols = reactive({
      if(is.null(input$hide_common) || input$hide_common==0) return(NULL)
      get_common_cols(lookup) %>% 
        filter(n_datasets/nrow(lookup) > input$hide_common/100) %>% 
        filter(!column %in% c(get_subjid_cols(lookup), get_crfname_cols(lookup))) %>% 
        pull(column)
    })
    
    #on row selected: show datatable ----
    observeEvent(input$input_table_rows_selected, {
      selected = input$input_table_rows_selected
      dataset_selected(names(datasets[selected]))
    })
    
    #on Reset button: reset subjid choice ----
    observeEvent(input$reset_subjid, {
      updateSelectInput(session, "subjid_selected", choices=c(ids))
    })
    
    #on Search type change: update label ----
    observeEvent(input$search_type_value, {
      update_switch("search_type_value", label=ifelse(input$search_type_value, "for value", "for column"))
    })
    
    #on Button Data Info: show infos ----
    observeEvent(input$btn_data_info, {
      dataset = dataset_selected()
      showModal(viewer_info_modal(dataset))
    })
    
    #on Button Settings: show the Settings dialog ----
    observeEvent(input$btn_settings, {      
      lookup_tbl = data_sidebar()
      n_filtered = lookup_tbl %>% filter(exclude) %>% nrow()
      showModal(viewer_settings_modal(
        hide_filtered_default = input$hide_filtered %0% FALSE,  
        n_filtered = n_filtered,
        hide_common_default = input$hide_common %0% 0
      ))
    })
    
    #on Button Search: show the Search dialog ----
    observeEvent(input$btn_search, {
      showModal(viewer_search_modal())
    })
    
    #on Search validation: show results ----
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
    
    #on Button Summary: show modal ----
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
    
    #output: datatable header text ----
    output$dataset_name = renderText({
      if(is.null(dataset_selected())) return("Loading")
      dataset_selected()
    })
    output$dataset_dim = renderText({
      if(is.null(dataset_selected())) return("")
      a = lookup %>% filter(dataset==dataset_selected())
      glue("{a$nrow} x {a$ncol}")
    })
    output$dataset_subj = renderText({
      if(is.null(dataset_selected())) return("")
      a = lookup %>% filter(dataset==dataset_selected())
      glue("{a$n_id} patients")
    })
    output$dataset_layout = renderText({
      if(is.null(dataset_selected())) return("")
      a = lookup %>% filter(dataset==dataset_selected())
      ifelse(a$rows_per_id>1, glue("long ({a$rows_per_id} rows per patient)"), "wide")
    })
    
    #output: hide common columns helper text ----
    output$hide_common_result = renderText({
      x = hidden_common_cols()
      if(is.null(x)) return("")
      glue("Columns hidden: {paste(x, collapse=', ')}")
    })
    
    #output: data information modal on button click ----
    output$data_info = renderDT({
      x = datasets[[dataset_selected()]]
      tibble(
        "Index"=seq(ncol(x)), 
        "Column name"=names(x), 
        "Label"=unlist(get_label(x)),
        "NA"=map_dbl(x, ~mean(is.na(.x)))
      ) %>% 
        datatable(
          rownames = FALSE,
          filter = "top",
          options = lst(
            scrollX = TRUE, scrollY = "66vh",
            pageLength = 500,
            dom = "t", #table only
          )
        ) %>%
        DT::formatPercentage(columns = 4, digits = 1)
    })
    
    #reactive for [output: sidebar data choice list] ----
    data_sidebar = reactive({
      selected_subjid = input$subjid_selected
      all_subjid = unlist(lookup$subjids) %>% unique() %>% sort()
      if(is.null(selected_subjid)) selected_subjid = all_subjid
      lookup %>% 
        mutate(
          has_subjid = map_lgl(subjids, ~any(selected_subjid %in% .x)),
          is_error = !is.na(crfname) & crfname=="** Error in source file **",
          exclude = !has_subjid | is_error,
          row_color = case_when(!has_subjid~"red", is_error~"grey", .default=NA)
        )
    })
    
    #reactive for [output: datatable body] ----
    data_current = reactive({
      req(dataset_selected())
      dataset = datasets[[dataset_selected()]]
      if (is.null(dataset)) return(tibble())
      
      hidden = input$hidden_hide %>% stringr::str_split_1("___")
      subjid_selected = input$subjid_selected
      all_selected = length(subjid_selected) == 0
      no_problem = length(subjid_cols) == 0
      dataset %>%
        select(-any_of(hidden_common_cols()), -any_of(hidden)) %>% 
        relocate(any_of2(subjid_cols), .before=1) %>% 
        arrange(pick(any_of2(subjid_cols))) %>% 
        filter(
           no_problem | all_selected |
            if_any(any_of2(subjid_cols), ~ .x %in% subjid_selected)
        )
    })

    #output: sidebar data choice list ----
    output$input_table = renderDT({
      selected_subjid = input$subjid_selected
      lookup_tbl = data_sidebar()
      if(isTRUE(input$hide_filtered)){
        lookup_tbl = lookup_tbl %>% filter(!exclude)
      }    
      sidebar_datatable(lookup_tbl)
    })

    #output: datatable body ----
    output$table = renderDT({      
      data = data_current()
      
      ##ContextMenu: Fixed Column ----
      fixed = input$hidden_fixed %>% stringr::str_split_1("___")
      fixed = c(subjid_cols, fixed, input$hidden_group, input$hidden_color) %>% unique()
      fixed = fixed[nzchar(fixed)] %>% intersect(names(data))
      data = data %>% 
        relocate(any_of2(fixed), .before=1)
      
      ##ContextMenu: Row Group ----
      i = which(names(data)==input$hidden_group)
      row_group = list(dataSrc = i)
      if(length(i)==0) row_group=NULL
      
      ##ContextMenu: Row Color ----
      col_color = data[[input$hidden_color]]
      row_style = row_style_col = NULL
      if(!is.null(col_color)){
        lvl = levels(factor(col_color) %>% forcats::fct_na_value_to_level())
        pal = scales::viridis_pal(alpha=0.5)(length(lvl))
        row_style = styleEqual(levels = lvl, values=pal)
        row_style_col = input$hidden_color
      }
      
      main_datatable(
        data = data,
        fixed = fixed,
        row_group = row_group,
        row_style = row_style,
        row_style_col = row_style_col
      )
    })
  }
}
