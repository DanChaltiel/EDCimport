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
    if(length(seed) != 1 || is.na(seed)){
      cli_abort("{.arg seed} must be a single non-missing value or {.val NULL}.")
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
  if(lower > upper) c(lower, upper) = c(upper, lower)
  if(n == 0) return(integer())
  as.integer(floor(runif(n, min = lower, max = upper + 1)))
}


.dummy_generate_numeric = function(n, param1, param2){
  lower = .dummy_number(param1, 0)
  upper = .dummy_number(param2, 1)
  if(lower > upper) c(lower, upper) = c(upper, lower)
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


.dummy_subject_ids = function(n){
  n = as.integer(n)
  if(n <= 0) return(character())
  width = max(4L, nchar(as.character(n)))
  paste0("DUMMY_SUBJECT_", sprintf(paste0("%0", width, "d"), seq_len(n)))
}


.dummy_validate_spec = function(spec){
  if(!is.data.frame(spec)){
    cli_abort("{.arg spec} must be a data frame.")
  }

  required = c(
    "dataset", "dataset_label", "n_rows", "n_subjects", "column",
    "column_label", "class", "generator", "depends_on", "param1", "param2",
    "param3", "missing_prop"
  )
  missing_columns = required[!required %in% names(spec)]
  if(length(missing_columns) > 0){
    cli_abort("{.arg spec} is missing required column{?s}: {.val {missing_columns}}.")
  }
  if(nrow(spec) == 0){
    cli_abort("{.arg spec} must contain at least one row.")
  }

  dataset = as.character(spec$dataset)
  column = as.character(spec$column)
  generator = as.character(spec$generator)
  if(any(is.na(dataset) | dataset == "") || any(is.na(column) | column == "")){
    cli_abort("{.arg spec} contains missing dataset or column names.")
  }
  if(anyDuplicated(paste(dataset, column, sep = "\r"))){
    cli_abort("{.arg spec} contains duplicated dataset/column pairs.")
  }

  allowed_generators = c(
    "identifier", "categorical", "logical", "integer", "numeric",
    "constant", "date", "text", "unsupported"
  )
  bad_generators = unique(generator[!generator %in% allowed_generators])
  if(length(bad_generators) > 0){
    cli_abort("{.arg spec} contains unsupported generator{?s}: {.val {bad_generators}}.")
  }

  for(dataset_name in unique(dataset)){
    rows = spec[dataset == dataset_name, , drop = FALSE]
    n_rows = unique(suppressWarnings(as.numeric(rows$n_rows)))
    n_subjects = unique(suppressWarnings(as.numeric(rows$n_subjects)))
    if(length(n_rows) != 1 || is.na(n_rows) || n_rows < 0 || n_rows != floor(n_rows)){
      cli_abort("Dataset {.val {dataset_name}} has an invalid {.field n_rows} value.")
    }
    if(length(n_subjects) != 1 || is.na(n_subjects) || n_subjects < 0 || n_subjects != floor(n_subjects)){
      cli_abort("Dataset {.val {dataset_name}} has an invalid {.field n_subjects} value.")
    }
  }

  missing_prop = suppressWarnings(as.numeric(spec$missing_prop))
  if(any(is.na(missing_prop)) || any(missing_prop < 0 | missing_prop > 1)){
    cli_abort("{.field missing_prop} must contain values between 0 and 1.")
  }

  invisible(spec)
}


.dummy_label = function(x){
  label = attr(x, "label", exact = TRUE)
  if(is.null(label) || length(label) == 0 || is.na(label[1])) return(NA_character_)
  as.character(label[1])
}


.dummy_scalar_character = function(x){
  if(length(x) == 0 || is.na(x[1])) return(NA_character_)
  as.character(x[1])
}


.dummy_number = function(x, default){
  value = suppressWarnings(as.numeric(x))
  if(length(value) == 0 || is.na(value[1]) || !is.finite(value[1])) return(default)
  value[1]
}


.dummy_has_class = function(class_string, value){
  if(length(class_string) == 0 || is.na(class_string)) return(FALSE)
  value %in% strsplit(class_string, "|", fixed = TRUE)[[1]]
}


#' @importFrom utils URLdecode URLencode
.dummy_encode_values = function(x){
  encoded = URLencode(as.character(x), reserved = TRUE)
  paste0(length(encoded), ":", paste(encoded, collapse = "|"))
}


.dummy_decode_values = function(x){
  if(length(x) == 0 || is.na(x) || !grepl(":", x, fixed = TRUE)) return(character())
  n = suppressWarnings(as.integer(sub(":.*$", "", x)))
  if(is.na(n) || n <= 0) return(character())
  payload = sub("^[^:]*:", "", x)
  values = strsplit(payload, "|", fixed = TRUE)[[1]]
  if(length(values) < n) values = c(values, rep("", n - length(values)))
  URLdecode(values[seq_len(n)])
}
