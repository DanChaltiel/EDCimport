


#' Patient gridplot
#' 
#' Draw a gridplot giving, for each patient and each dataset, whether the 
#' patient is present in the dataset. Data are drawn from [get_datasets].
#'
#' @param sort_rows whether to sort patients from "present in most datasets" to "present in least datasets"
#' @param sort_cols whether to sort datasets from "containing the most patients" to "containing the least patients"
#' @param axes_flip whether to flip the axes, so that patients are on the Y axis and datasets on the X axis
#' @param show_grid whether to show the grid
#' @param preprocess a function to preprocess the patient ID, e.g. `as.numeric`, or a custom function with string replacement
#'
#' @return a `ggplot` object
#' @export
#'
#' @examples
#' \dontrun{
#'   tm = read_trialmaster("path/to/archive.zip")
#'   load_list(tm)
#'   edc_patient_gridplot(sort_rows=FALSE, sort_cols=FALSE)
#'   edc_patient_gridplot(axes_flip=TRUE, show_grid=TRUE,
#'                        preprocess=~str_remove(.x, "\\D*")) #remove all non-digits
#' }
#' @importFrom cli format_inline
#' @importFrom dplyr arrange bind_rows mutate
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 aes coord_equal element_text geom_tile ggplot labs scale_fill_manual scale_x_discrete theme theme_minimal
#' @importFrom purrr map map_dbl
#' @importFrom rlang as_function is_formula
#' @importFrom tibble tibble
edc_patient_gridplot = function(sort_rows=TRUE, sort_cols=TRUE, 
                                axes_flip=FALSE, show_grid=TRUE, preprocess=NULL,
                                palette=c("Yes"="#00468BFF", "No"="#ED0000FF")){
  
  data_list = get_datasets()
  subjid_cols = get_subjid_cols()
  if(is.null(preprocess)) preprocess=identity
  if(is_formula(preprocess)) preprocess=as_function(preprocess)
  
  subjid_list = data_list %>% 
    map(~{
      .get_subjid_vector(.x, subjid_cols)$subjid %>% unique() %>% preprocess()
    })
  
  all_subjid = subjid_list %>% unlist() %>% unique() %>% sort()
  
  nrow_rslt = length(all_subjid) * length(data_list)
  df = subjid_list %>% 
    map(~{
      tibble(subjid=all_subjid,
             included=map_dbl(all_subjid, function(s) sum(s==.x)))
    }) %>% 
    bind_rows(.id="dataset") %>% 
    arrange(dataset) %>% 
    mutate(included = ifelse(included>0, 1, 0)) %>% 
    mutate(subjid_sum = sum(included), .by=subjid) %>% 
    mutate(dataset_sum = sum(included), .by=dataset)
  
  stopifnot(nrow(df) == nrow_rslt)
  
  
  l = edc_lookup()
  extraction = attr(l, "datetime_extraction")
  project_name = attr(l, "project_name")
  par_extraction = par_projname = NULL
  if(!is.null(project_name)) 
    par_projname = format_inline(" - {project_name}")
  if(!is.null(extraction)) 
    par_extraction = format_inline(" (extraction of {format_ymd(extraction)}) ")
  plot_title = format_inline("Patient gridplot{par_projname}")
  plot_subtitle = format_inline("(extraction of {format_ymd(extraction)})")
  
  
  df = df %>% 
    arrange(dataset) %>% 
    mutate(included = ifelse(included>0, 1, 0)) %>% 
    mutate(subjid_sum = sum(included), .by=subjid) %>% 
    mutate(dataset_sum = sum(included), .by=dataset)
  
  cur_aes = aes(x=subjid, y=dataset, fill=included)
  custom_desc = TRUE
  if(axes_flip) {
    cur_aes = aes(x=dataset, y=subjid, fill=included)
    custom_desc = FALSE
  }
  
  if(isFALSE(show_grid)) show_grid = 0
  if(isTRUE(show_grid)) show_grid = "black"
  
  df %>% 
    mutate(
      included = factor(included, levels=c(0,1), labels=c("No", "Yes")),
      subjid = factor(subjid, levels=mixedsort(unique(subjid), decreasing=TRUE)),
      dataset = factor(dataset, levels=mixedsort(unique(dataset))),
      subjid = if(sort_rows) fct_reorder(subjid, subjid_sum, .desc=custom_desc) else subjid,
      dataset = if(sort_cols) fct_reorder(dataset, dataset_sum, .desc=!custom_desc) else dataset,
    ) %>% 
    ggplot() +
    cur_aes +
    geom_tile(color=show_grid) +
    scale_x_discrete(position = "top") +
    scale_fill_manual(values=palette) +
    labs(y="Patient ID", x=NULL, fill="Included", color="Included",
         title=plot_title, subtitle=plot_subtitle) +
    coord_equal() +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90L, hjust=0),
    )
  
}


.get_subjid_vector = function(df, subjid_cols){
  rtn = df %>% 
    select(subjid=any_of2(subjid_cols)) %>% 
    as_tibble()
  if(ncol(rtn) == 0) rtn = tibble(subjid=character(0))
  rtn
}


