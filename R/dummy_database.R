#' Generate a dummy EDC database from a specification
#'
#' `edc_dummy_database()` only uses the portable specification produced by
#' [edc_dummy_spec()]. It does not require access to the original database.
#'
#' The generated database uses artificial subject identifiers and a fixed
#' artificial extraction date. Its `.lookup` table is rebuilt from the dummy
#' datasets. The current implementation preserves only simple univariate
#' structure and should not be interpreted as an anonymisation or synthetic-data
#' method preserving clinical or statistical relationships.
#'
#' @param spec A dummy specification produced by [edc_dummy_spec()], including
#'   after a CSV round-trip.
#' @param seed Optional random seed. With the same specification and seed, the
#'   generated database is reproducible. The caller's RNG state is restored on
#'   exit.
#'
#' @return An object with classes `edc_dummy` and `edc_database`.
#' @export
edc_dummy_database = function(spec, seed = NULL){
  .dummy_validate_spec(spec)
  spec = as.data.frame(spec, stringsAsFactors = FALSE)

  if(!is.null(seed)){
    if(!is.numeric(seed) || length(seed) != 1 || is.na(seed)){
      cli_abort("{.arg seed} must be a single non-missing numeric value or {.val NULL}.")
    }
  }

  had_random_seed = exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if(had_random_seed){
    old_random_seed = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if(had_random_seed){
      assign(".Random.seed", old_random_seed, envir = .GlobalEnv)
    } else if(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)){
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  if(!is.null(seed)) set.seed(seed)

  dataset_names = unique(as.character(spec$dataset))
  datalist = lapply(dataset_names, function(dataset_name){
    dataset_spec = spec[spec$dataset == dataset_name, , drop = FALSE]
    .dummy_generate_dataset(dataset_spec)
  })
  names(datalist) = dataset_names

  dummy = new_edc_database(
    datalist,
    datetime_extraction = as.POSIXct("2000-01-01 00:00:00", tz = "UTC"),
    extend_lookup = NULL,
    set_lookup = FALSE,
    dummy = TRUE
  )
  class(dummy) = c("edc_dummy", "edc_database")
  dummy
}


.dummy_generate_dataset = function(spec){
  n_rows = unique(as.integer(spec$n_rows))
  n_subjects = unique(as.integer(spec$n_subjects))
  ids = .dummy_subject_ids(n_subjects)

  columns = lapply(seq_len(nrow(spec)), function(i){
    row = spec[i, , drop = FALSE]
    generator = as.character(row$generator)
    class_string = as.character(row$class)
    param1 = .dummy_scalar_character(row$param1)
    param2 = .dummy_scalar_character(row$param2)

    x = switch(
      generator,
      identifier = rep(ids, length.out = n_rows),
      categorical = .dummy_generate_categorical(n_rows, class_string, param1),
      logical = runif(n_rows) < .dummy_number(param1, 0.5),
      integer = .dummy_generate_integer(n_rows, param1, param2),
      numeric = .dummy_generate_numeric(n_rows, param1, param2),
      constant = .dummy_generate_constant(n_rows, class_string, param1),
      date = .dummy_generate_date(n_rows, param1, param2),
      text = paste("dummy text", seq_len(n_rows)),
      unsupported = rep(NA, n_rows),
      cli_abort("Unsupported dummy generator {.val {generator}}.")
    )

    if(generator != "identifier" && n_rows > 0){
      missing_prop = suppressWarnings(as.numeric(row$missing_prop))
      if(is.na(missing_prop)) missing_prop = 0
      n_missing = round(n_rows * min(1, max(0, missing_prop)))
      if(n_missing > 0){
        x[sample.int(n_rows, n_missing, replace = FALSE)] = NA
      }
    }

    label = .dummy_scalar_character(row$column_label)
    if(!is.na(label)) attr(x, "label") = label
    x
  })
  names(columns) = as.character(spec$column)

  data = as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
  dataset_label = .dummy_scalar_character(spec$dataset_label[1])
  if(!is.na(dataset_label)) attr(data, "label") = dataset_label
  data
}


.dummy_generate_categorical = function(n, class_string, param1){
  values = .dummy_decode_values(param1)
  if(length(values) == 0) values = c("level_1", "level_2")
  sampled = if(n == 0) character() else sample(values, n, replace = TRUE)

  if(.dummy_has_class(class_string, "factor")){
    return(factor(
      sampled,
      levels = values,
      ordered = .dummy_has_class(class_string, "ordered")
    ))
  }
  sampled
}


.dummy_generate_integer = function(n, param1, param2){
  lower = .dummy_number(param1, 0)
  upper = .dummy_number(param2, 10)
  if(lower > upper){
    tmp = lower
    lower = upper
    upper = tmp
  }
  lower = ceiling(lower)
  upper = floor(upper)
  if(lower > upper) lower = upper
  if(n == 0) return(integer())
  if(lower == upper) return(rep(as.integer(lower), n))
  as.integer(floor(runif(n, min = lower, max = upper + 1)))
}


.dummy_generate_numeric = function(n, param1, param2){
  lower = .dummy_number(param1, 0)
  upper = .dummy_number(param2, 1)
  if(lower > upper){
    tmp = lower
    lower = upper
    upper = tmp
  }
  if(n == 0) return(numeric())
  if(lower == upper) return(rep(as.numeric(lower), n))
  runif(n, min = lower, max = upper)
}


.dummy_generate_constant = function(n, class_string, param1){
  if(.dummy_has_class(class_string, "logical")){
    return(rep(identical(toupper(param1), "TRUE"), n))
  }
  if(.dummy_has_class(class_string, "integer")){
    return(rep(as.integer(.dummy_number(param1, 0)), n))
  }
  if(.dummy_has_class(class_string, "numeric") || .dummy_has_class(class_string, "double")){
    return(rep(.dummy_number(param1, 0), n))
  }
  rep("dummy", n)
}


.dummy_generate_date = function(n, param1, param2){
  start = suppressWarnings(as.Date(param1))
  if(is.na(start)) start = as.Date("2000-01-01")
  span = max(0L, as.integer(.dummy_number(param2, 365)))
  if(n == 0) return(start + integer())
  start + sample.int(span + 1L, n, replace = TRUE) - 1L
}
