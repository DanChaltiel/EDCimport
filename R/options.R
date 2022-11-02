
#TODO edc_options()
edc_options = function(trialmaster_pw, .local=FALSE){
  
  argg = as.list(match.call())[-1]
  
  
  dup = names(argg) %in% paste0("crosstable_", names(argg))
  dup_args = names(argg)[dup]
  argg = argg[!names(argg) %in% dup_args]
  prefix = ifelse(str_starts(names(argg), "crosstable_"), "", "crosstable_")
  names(argg) = paste0(prefix, names(argg))
  if(length(dup_args)>0){
    cli::cli_warn(c("Duplicated crosstable option{?s} were ignored: {dup_args}.",
                    i='You can now remove the old "crosstable_" prefix.'),
                  class="crosstable_dupl_option_warning")
  }
  
  if(.local){
    argg = c(argg, .frame = caller_env())
    do.call(local_options, argg)
  }
  else do.call(options, argg)
  invisible()
}