


#' Patient gridplot
#' 
#' Draw a gridplot giving, for each patient and each dataset, whether the 
#' patient is present in the dataset. Data are drawn from [get_datasets].
#'
#' @param sort_rows whether to sort patients from "present in most datasets" to "present in least datasets"
#' @param sort_cols whether to sort datasets from "containing the most patients" to "containing the least patients"
#' @param gradient whether to add a color gradient for repeating measures
#' @param axes_flip whether to flip the axes, so that patients are on the Y axis and datasets on the X axis
#' @param show_grid whether to show the grid
#' @param preprocess a function to preprocess the patient ID, e.g. `as.numeric`, or a custom function with string replacement
#' @param palette the colors to use
#' @param datasets,lookup internal
#'
#' @return a `ggplot` object
#' @export
#'
#' @examples
#' \dontrun{
#'   tm = read_trialmaster("path/to/archive.zip")
#'   load_database(db)
#'   edc_patient_gridplot(sort_rows=FALSE, sort_cols=FALSE)
#'   edc_patient_gridplot(axes_flip=TRUE, show_grid=TRUE,
#'                        preprocess=~str_remove(.x, "\\D*")) #remove all non-digits
#' }
#' @importFrom cli format_inline
#' @importFrom dplyr arrange bind_rows mutate
#' @importFrom forcats fct_reorder
#' @importFrom ggplot2 aes coord_equal element_text geom_tile ggplot labs scale_fill_gradientn scale_fill_manual scale_x_discrete theme theme_minimal
#' @importFrom purrr imap map map_dbl
#' @importFrom rlang as_function is_formula
#' @importFrom tibble tibble
edc_patient_gridplot = function(sort_rows=TRUE, sort_cols=TRUE, gradient=FALSE,
                                axes_flip=FALSE, show_grid=TRUE, preprocess=NULL,
                                palette=c("Yes"="#00468BFF", "No"="#ED0000FF"),
                                datasets=get_datasets(), lookup=edc_lookup()){
  subjid_cols = get_subjid_cols(lookup=lookup)
  datasets = keep(datasets, is.data.frame)
  if(is_formula(preprocess)) preprocess = as_function(preprocess)
  if(length(datasets)==0){
    cli_abort("No datasets are availables", .internal=TRUE)
  }
  if(length(subjid_cols)==0){
    cli_abort("Datasets must have a subject identifier for {.fn edc_patient_gridplot} to work.")
  }
  
  subjid_list = datasets %>% 
    imap(~{
      tmp = .get_subjid_vector(.x, subjid_cols)$subjid
      tmp = tmp[!is.na(tmp)]
      if(length(tmp)==0) return(NULL)
      if(!is.null(preprocess)) tmp = preprocess(tmp)
      else if(can_be_numeric(tmp)) tmp = as.numeric(tmp)
      if(!gradient) tmp = unique(tmp)
      tmp
    })
  
  all_subjid = subjid_list %>% unlist() %>% unique() %>% unname() %>% sort()
  
  nrow_rslt = length(all_subjid) * length(datasets)
  df = subjid_list %>% 
    imap(~{
      isum = map_dbl(all_subjid, function(s) sum(s==.x))
      if(is.null(.x)) isum = 0
      tibble(subjid=all_subjid, included_sum=isum)
    }) %>% 
    bind_rows(.id="dataset") %>% 
    arrange(dataset) %>% 
    mutate(included = ifelse(included_sum>0, 1, 0)) %>% 
    mutate(subjid_sum = sum(included), .by=any_of("subjid")) %>% 
    mutate(dataset_sum = sum(included), .by=dataset) %>% 
    mutate(
      included = factor(included, levels=c(0,1), labels=c("No", "Yes")),
      subjid = factor(subjid, levels=mixedsort(unique(subjid), decreasing=TRUE)),
      dataset = factor(dataset, levels=mixedsort(unique(dataset))),
    )
  stopifnot(nrow(df) == nrow_rslt)
  stopifnot(!any(is.na(df$subjid_sum)))
  
  extraction = attr(lookup, "datetime_extraction")
  project_name = attr(lookup, "project_name")
  par_projname = plot_subtitle = NULL
  if(!is.null(project_name)) 
    par_projname = format_inline(" - {project_name}")
  if(!is.null(extraction)) 
    plot_subtitle = format_inline(" (extraction of {format_ymd(extraction)}) ")
  plot_title = format_inline("Patient gridplot{par_projname}")
  
  
  if(isFALSE(show_grid)) show_grid = 0
  if(isTRUE(show_grid)) show_grid = "black"
  if(isTRUE(axes_flip)) {
    aes_x = "dataset"
    aes_y = "subjid"
    custom_desc = FALSE
  } else {
    aes_x = "subjid"
    aes_y = "dataset"
    custom_desc = TRUE
  }
  if(isTRUE(gradient)){
    aes_fill = "included_sum"
    fill_name = "Number of forms per patient"
    val1 = 1/max(df$included_sum)
    scale_fill = scale_fill_gradientn(colours = c(palette["No"], palette["Yes"], "black"),
                                      values = c(0, val1, 1))
  } else {
    aes_fill = "included"
    fill_name = "Patient included"
    scale_fill = scale_fill_manual(values=palette)
  }

  if(sort_rows){
    df = df %>% 
      mutate(subjid = fct_reorder(subjid, subjid_sum, .desc=custom_desc, .na_rm = F))
  }
  if(sort_cols){
    df = df %>% 
      mutate(dataset = fct_reorder(dataset, dataset_sum, .desc=!custom_desc, .na_rm = F))
  }
  df %>% 
    ggplot() +
    aes(x=.data[[aes_x]], y=.data[[aes_y]], fill=.data[[aes_fill]]) +
    geom_tile(color=show_grid) +
    scale_x_discrete(position = "top") +
    scale_fill +
    labs(y="Patient ID", x=NULL, fill=fill_name, color="Included",
         title=plot_title, subtitle=plot_subtitle) +
    coord_equal() +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90L, hjust=0),
    )
  
}


#' @importFrom dplyr as_tibble select
#' @importFrom tibble tibble
.get_subjid_vector = function(df, subjid_cols){
  rtn = df %>% 
    select(subjid=any_of2(subjid_cols)) %>% 
    as_tibble()
  if(ncol(rtn) == 0) rtn = tibble(subjid=character(0))
  rtn
}
