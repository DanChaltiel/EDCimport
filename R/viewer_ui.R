
#' @importFrom cli format_inline
#' @importFrom purrr map_dbl
edc_viewer_ui = function(datasets, lookup){
  card=bslib::card;card_body=bslib::card_body;card_header=bslib::card_header;
  card_title=bslib::card_title;page_sidebar=bslib::page_sidebar;sidebar=bslib::sidebar;
  DTOutput=DT::DTOutput;
  actionButton=shiny::actionButton;selectInput=shiny::selectInput;actionLink=shiny::actionLink;
  div=shiny::div;HTML=shiny::HTML;tags=shiny::tags;textOutput=shiny::textOutput
  checkboxInput=shiny::checkboxInput;
  tooltip=bslib::tooltip; icon=shiny::icon; selectizeInput=shiny::selectizeInput;
  
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
  title_div = div(
    style = "width: 100%; display: flex; justify-content: space-between; align-items: center;",
    title, 
    div(
      actionButton("btn_search", icon=icon("search"), label=NULL) |> 
        tooltip("Search"),
      actionButton("btn_db_summary", icon=icon("circle-question"), label=NULL) |> 
        tooltip("Database summary")
    )
  )
  
  page_sidebar(
    window_title = paste(project_name, " - EDCimport"),
    title = title_div,
    height = "100vh",
    sidebar = sidebar(
      width = 350,
      card(
        card_title("Subjects:", actionButton("reset_subjid", "Reset", style="padding:5px")),
        selectizeInput("subjid_selected", label=NULL, choices=1, multiple=TRUE,
                       options = list("plugins"=list("remove_button"), 
                                      "create"=TRUE, "persist"=FALSE)),
      ),
      card(
        card_title("Select a dataset:", container = shiny::h3),
        checkboxInput("hide_filtered", "Hide empty tables"),
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
    
    tags$head(tags$style(
      '.card{overflow: visible !important;}',
      '.card-body{overflow: visible !important;}',
      '.modal-dialog{margin: 50px auto;}',
      '.modal-header{padding-bottom: 0;}',
      '.modal-title{width: 100%;}',
      '.bslib-input-switch{font-size: large;}',
    )), 
    
    tags$script(HTML(
      #Typing Enter in #search_input validate the input
      "$(document).on('keyup', '#search_input', function(e) {
        if (e.which == 13) {
          Shiny.setInputValue('search_validate', true, {priority: 'event'});
        }
      });",
      #Activate JQuery tooltips for Data headers
      "$(document).on('shiny:value', function(e) {
        if(e.name != 'table') return(null)
        setTimeout(function() {
          $('.edc_label').tooltip();
        }, 300);
      });"
    ))
  )
}

