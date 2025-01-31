#' Plot the populations
#' 
#' In a RCT, you usually have several populations of analysis, and this function allow to show 
#' which patient is in which population graphically.
#'
#' @param x a named list of subject ID, as numeric or factor.
#' @param id_per_row number of patients per rows.
#' @param ref the whole population. Default to the first member of `x`.
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' 
#' #in real word code, use filter and pull to get these vectors
#' pop_total = c(1:180) %>% setdiff(55) #screen failure, no patient 55
#' pop_itt = pop_total %>% setdiff(10) #patient 10 has had the wrong treatment
#' pop_safety = pop_total %>% setdiff(c(40,160)) #patients 40 and 160 didn't receive any treatment
#' pop_m_itt = pop_total %>% setdiff(c(40,160,80)) #patient 80 had a wrong inclusion criterion
#' pop_evaluable = pop_total %>% setdiff(c(40,160,101,147,186)) #patients with no recist evaluation
#' 
#' l = list(
#'   "Total population"=pop_total,
#'   "ITT population"=pop_itt,
#'   "Safety population"=pop_safety,
#'   "mITT population"=pop_m_itt,
#'   "Evaluable population"=pop_evaluable
#' )
#' edc_population_plot(l)
#' edc_population_plot(l[-1], ref=pop_total)
#' edc_population_plot(l, ref=1:200)
#' edc_population_plot(l, id_per_row=60)
#' @importFrom cli cli_abort
#' @importFrom dplyr bind_cols last mutate
#' @importFrom ggplot2 aes element_blank facet_wrap geom_tile ggplot labs theme
#' @importFrom purrr iwalk map
#' @importFrom rlang is_named
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
edc_population_plot = function(x, id_per_row=50, ref="first"){
  assert_class(x, "list")
  assert_class(id_per_row, c("numeric", "integer", "double"))
  assert(is_named(x))
  x = map(x, as.numeric)
  if(identical(ref, "first")) ref = x[[1]]
  assert_class(ref, c("numeric", "integer", "double"))
  
  iwalk(x, ~{
    if(anyNA(.x)) cli_abort("There should not be missing data in list member {.val {.y}}",
                            class="edc_population_plot_na_error")
  })
  
  y = x %>% map(~ref %in% .x) %>% bind_cols()
  range_sup = ceiling(max(ref) / id_per_row) * id_per_row
  breaks = seq(0, range_sup, by=id_per_row)
  
  df = tibble(subjid=ref, !!!y) %>% 
    pivot_longer(-subjid) %>% 
    mutate(group=cut(subjid, breaks=breaks),
           name=factor(name, levels=rev(names(x))),
           value=fct_yesno(value))
  dummy = tibble(subjid=range_sup, name=last(df$name), 
                 value=last(df$value), group=last(df$group))
  df %>% 
    ggplot(aes(x=subjid, y=name, fill=value)) +
    geom_tile(color="black") +
    geom_tile(data=dummy, fill="transparent") +
    facet_wrap(~group, ncol=1, scales="free_x", strip.position="right") +
    labs(x="Patient number", y=NULL, fill="Patient included") +
    theme(legend.position="top",
          strip.background = element_blank(),
          strip.text.x = element_blank())
}
