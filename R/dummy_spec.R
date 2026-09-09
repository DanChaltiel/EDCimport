#' Create a portable dummy-data specification
#'
#' `edc_dummy_spec()` profiles the structure of an `edc_database` into a flat,
#' CSV-compatible data frame. The specification is intended to be editable and
#' transportable without the original patient-level database.
#'
#' The current implementation is deliberately minimal. It preserves dataset and
#' column structure, basic R classes and labels, coarse univariate generation
#' parameters, and missingness. It does not preserve clinical relationships,
#' patient trajectories, or mixed-grain dependencies.
#'
#' Real subject identifiers, character values, and calendar dates are not copied
#' into the specification. Factor levels are treated as structural metadata and
#' are currently preserved.
#'
#' @param db An `edc_database`.
#'
#' @return A plain `data.frame` that can be written to CSV and read back with
#'   [utils::write.csv()] and [utils::read.csv()].
#' @export
edc_dummy_spec = function(db){
  if(!inherits(db, "edc_database")){
    cli_abort("{.arg db} must be an {.cls edc_database}.")
  }

  dataset_names = names(db)[vapply(db, is.data.frame, logical(1))]
  dataset_names = dataset_names[dataset_names != ".lookup"]
  if(length(dataset_names) == 0){
    cli_abort("{.arg db} does not contain any dataset.")
  }

  specs = lapply(dataset_names, function(dataset_name){
    data = db[[dataset_name]]
    id_index = which(toupper(names(data)) == "SUBJID")
    n_subjects = nrow(data)
    if(length(id_index) > 0){
      ids = data[[id_index[1]]]
      n_subjects = length(unique(ids[!is.na(ids)]))
      if(nrow(data) > 0 && n_subjects == 0) n_subjects = 1L
    }

    rows = lapply(names(data), function(column_name){
      x = data[[column_name]]
      profile = .dummy_profile_column(column_name, x)
      data.frame(
        dataset = dataset_name,
        dataset_label = .dummy_label(data),
        n_rows = nrow(data),
        n_subjects = n_subjects,
        column = column_name,
        column_label = .dummy_label(x),
        class = paste(class(x), collapse = "|"),
        generator = profile$generator,
        depends_on = NA_character_,
        param1 = profile$param1,
        param2 = profile$param2,
        param3 = profile$param3,
        missing_prop = if(length(x) == 0) 0 else mean(is.na(x)),
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, rows)
  })

  rtn = do.call(rbind, specs)
  rownames(rtn) = NULL
  rtn
}


.dummy_profile_column = function(column_name, x){
  missing = list(param1 = NA_character_, param2 = NA_character_, param3 = NA_character_)

  if(toupper(column_name) == "SUBJID"){
    return(c(list(generator = "identifier"), missing))
  }

  if(inherits(x, "Date")){
    return(list(
      generator = "date",
      param1 = "2000-01-01",
      param2 = "365",
      param3 = NA_character_
    ))
  }

  if(is.factor(x)){
    return(list(
      generator = "categorical",
      param1 = .dummy_encode_values(levels(x)),
      param2 = NA_character_,
      param3 = NA_character_
    ))
  }

  if(is.logical(x)){
    y = x[!is.na(x)]
    if(length(y) > 0 && length(unique(y)) == 1){
      return(list(
        generator = "constant",
        param1 = as.character(y[1]),
        param2 = NA_character_,
        param3 = NA_character_
      ))
    }
    p = if(length(y) == 0) 0.5 else mean(y)
    return(list(
      generator = "logical",
      param1 = as.character(p),
      param2 = NA_character_,
      param3 = NA_character_
    ))
  }

  if(is.integer(x)){
    y = x[!is.na(x)]
    if(length(y) > 0 && length(unique(y)) == 1){
      return(list(
        generator = "constant",
        param1 = as.character(y[1]),
        param2 = NA_character_,
        param3 = NA_character_
      ))
    }
    bounds = if(length(y) == 0) c(0L, 10L) else range(y)
    return(list(
      generator = "integer",
      param1 = as.character(bounds[1]),
      param2 = as.character(bounds[2]),
      param3 = NA_character_
    ))
  }

  if(is.numeric(x)){
    y = x[!is.na(x)]
    if(length(y) > 0 && length(unique(y)) == 1){
      return(list(
        generator = "constant",
        param1 = as.character(y[1]),
        param2 = NA_character_,
        param3 = NA_character_
      ))
    }
    bounds = if(length(y) == 0) c(0, 1) else range(y)
    return(list(
      generator = "numeric",
      param1 = as.character(bounds[1]),
      param2 = as.character(bounds[2]),
      param3 = NA_character_
    ))
  }

  if(is.character(x)){
    y = unique(x[!is.na(x)])
    if(length(y) <= 10){
      categories = paste0("category_", seq_len(max(1, length(y))))
      return(list(
        generator = "categorical",
        param1 = .dummy_encode_values(categories),
        param2 = NA_character_,
        param3 = NA_character_
      ))
    }
    return(list(
      generator = "text",
      param1 = "dummy text",
      param2 = NA_character_,
      param3 = NA_character_
    ))
  }

  c(list(generator = "unsupported"), missing)
}
