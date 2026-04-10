
# Datatables -------------------------------------------------------------

#' @noRd
#' @keywords internal
main_datatable = function(data, fixed, row_group, row_style, row_style_col) {
  data %>% 
    mutate(across(where(~ is.character(.x) || is.factor(.x)), 
           ~ str_remove_all(.x, "<.*?>"))) %>% #remove HTML tags
    DT::datatable(
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
    DT::formatStyle(
      columns = TRUE,
      `white-space` = "nowrap",
      `text-overflow` = "ellipsis",
      `overflow` = "hidden",
      # `max-width` = "150px",
      `height` = "20px"
    ) %>% 
    DT::formatStyle(
      columns = TRUE,
      valueColumns=row_style_col,
      backgroundColor = row_style
    ) %>% 
    DT::formatStyle(
      columns = fixed,
      backgroundColor = "white"
    )
}

#' @noRd
#' @keywords internal
sidebar_datatable = function(lookup){
  crf_name_i = which(names(lookup)=="crfname") - 1
  lookup %>% 
    as_tibble() %>% 
    DT::datatable(
      rownames = FALSE,
      selection = "single",
      filter = "none",
      options = lst(
        pageLength = 500,
        dom = "t",
        columnDefs = list(list(visible=FALSE, targets=seq(3, ncol(lookup)-1))),
        rowCallback = htmlwidgets::JS(
          "function(row, data) {",
            glue("$('td', row).attr('title', data[{crf_name_i}]).addClass('edc_label');"),
          "}"
        ),
      )
    ) %>% 
    DT::formatStyle(
      columns = TRUE,
      valueColumns="row_color",
      color = DT::styleEqual(levels = c("red", "grey"), values = c("red", "grey")),
      `white-space` = "nowrap",
      `height` = "20px"
    )
}

# Modals -----------------------------------------------------------------

#' @noRd
#' @keywords internal
viewer_settings_modal = function(hide_filtered_default, n_filtered, hide_common_default) {
    #   observe({
    #   data = data_sidebar()
    #   n_filtered = data %>% filter(exclude) %>% nrow()
    #   updateCheckboxInput(session, "hide_filtered", 
    #                       label=glue("Hide empty datasets in the side panel (N={n_filtered})"))
    # })
  hide_filtered_label = glue("Hide empty datasets in the side panel (N={n_filtered})")
  shiny::modalDialog(
    id = "modal_settings",
    title = "Settings",
    bslib::card(
      shiny::checkboxInput(
        "hide_filtered",
        hide_filtered_label,
        value = hide_filtered_default
      )
    ),
    bslib::card(
      shiny::sliderInput(
        "hide_common",
        "Hide columns shared by this proportion of datasets (set to 0 to disable)",
        min = 0,
        max = 100,
        value = hide_common_default,
        post = " %"
      ),
      shiny::textOutput("hide_common_result")
    ),
    easyClose = TRUE,
    footer = NULL
  )
}

#' @noRd
#' @keywords internal
viewer_info_modal = function(dataset) {
  shiny::modalDialog(
    id="modal_data_info",
    title = glue("Details for dataset `{dataset}`"),
    size="xl",
    bslib::card(
      style="height:75vh; overflow:auto;",
      DT::DTOutput("data_info")
    ),
    easyClose = TRUE,
    footer = NULL
  )
}

#' @noRd
#' @keywords internal
viewer_search_modal = function() {
  shiny::modalDialog(
    title = shiny::div(
      id = "modal_search_header",
      style = c(
        "width: 100%; display: flex; justify-content: space-between;",
        "gap: 50px; align-items: center;"
      ),
      shiny::div(
        style = "display: flex; flex-grow: 1; align-items: center;",
        shiny::textInput(
          "search_input",
          label = NULL,
          placeholder = "Enter a keyword",
          width = "100%"
        )
      ),
      shiny::div(
        style = "display: flex; align-items: center; gap: 10px;",
        shiny::div(
          style = "display: flex; align-items: center; height: 38px;",
          bslib::input_switch("search_type_value", "for column", width = "175px")
        ),
        shiny::actionButton("search_validate", "Search", icon = shiny::icon("search"))
      )
    ),
    shiny::uiOutput("search_error"),
    DT::DTOutput("search_result"),
    easyClose = TRUE,
    size = "xl",
    footer = NULL
  )
}


# Utils ---------------------------------------------------------------------------------------


#' Internal util to load functions without importFrom
#' @noRd
#' @examples
#' import_to_list("#' @importFrom bslib card card_body sidebar")
#' import_to_list("shiny actionButton selectInput actionLink tags textOutput")
#' @importFrom stringr str_remove
import_to_list = function(x){
  x = str_remove(x, "#' @importFrom ") %>% stringr::str_split_1(" ")
  paste0(x[-1], "=", x[1], "::", x[-1]) %>% paste(collapse=";")
}

#' @noRd
#' @keywords internal
#' @importFrom dplyr select
#' @importFrom purrr keep map
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


#' @noRd
#' @keywords internal
hide_first = function(){
  list(targets=0, visible=FALSE)
}

#' @importFrom glue glue
#' @importFrom purrr map_lgl
#' @noRd
#' @keywords internal
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
#' @keywords internal
#' @importFrom glue glue
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
