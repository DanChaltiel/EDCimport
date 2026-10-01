.dummy_identifier_class = function(classes){
  if(all(vapply(classes, function(x){
    .dummy_has_class(x, "integer") || .dummy_has_class(x, "numeric") ||
      .dummy_has_class(x, "double")
  }, logical(1)))) "integer" else "character"
}


.dummy_numeric_identifier = function(x){
  if(is.factor(x) || !(is.character(x) || is.numeric(x))) return(FALSE)
  x = x[!is.na(x)]
  if(length(x) == 0) return(TRUE)
  if(!isTRUE(can_be_numeric(x))) return(FALSE)
  all(is.finite(suppressWarnings(as.numeric(x))))
}


.dummy_subject_ids = function(n, class_string){
  n = as.integer(n)
  if(.dummy_has_class(class_string, "integer")) return(seq_len(n))
  if(n <= 0) return(character())
  width = max(4L, nchar(as.character(n)))
  paste0("DUMMY_SUBJECT_", sprintf(paste0("%0", width, "d"), seq_len(n)))
}


.dummy_structure_defaults = function(spec){
  defaults = list(
    unique = NA_character_, level = NA_integer_,
    mean_n = NA_real_, mean_rows = NA_real_
  )
  for(name in names(defaults)){
    if(!name %in% names(spec)) spec[[name]] = defaults[[name]]
  }
  spec$unique = as.character(spec$unique)
  spec$depends_on = as.character(spec$depends_on)
  for(name in c("level", "mean_n", "mean_rows")){
    spec[[name]] = as.character(spec[[name]])
  }
  spec
}


.dummy_group_ids = function(data, columns){
  if(nrow(data) == 0) return(integer())
  data %>%
    group_by(across(all_of(columns))) %>%
    dplyr::group_indices()
}


.dummy_check_structure = function(data, spec){
  for(i in seq_len(nrow(spec))){
    column = as.character(spec$column[i])
    keys = .dummy_decode_values(spec$depends_on[i])
    if(is.na(spec$level[i]) && length(keys) > 0 && nrow(data) > 0 &&
       !.dummy_is_dependency(data, keys, column) && !all(is.na(data[[column]]))){
      cli_abort("Dataset {.val {spec$dataset[i]}}: generated {.field {column}} violates its dependency. Edit the specification.")
    }
    constraints = if(identical(as.character(spec$unique[i]), "*")){
      list(column)
    } else lapply(.dummy_decode_values(spec$unique[i]), function(key) c(key, column))
    for(constraint in constraints){
      observed = data[complete.cases(data[constraint]), constraint, drop = FALSE]
      if(anyDuplicated(observed)){
        cli_abort("Dataset {.val {spec$dataset[i]}}: generated {.field {column}} violates uniqueness. Edit its dependencies, repetition means, or uniqueness constraints.")
      }
    }
  }
  invisible(data)
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
  spec = .dummy_structure_defaults(spec)

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
    if(n_rows > 0 && n_subjects == 0 && any(as.character(rows$generator) == "identifier")){
      cli_abort("Dataset {.val {dataset_name}} needs at least one subject for an identifier generator.")
    }
    levels = suppressWarnings(as.numeric(rows$level))
    if(any(!is.na(rows$level) & (is.na(levels) | !is.finite(levels) | levels < 1 | levels != floor(levels))) ||
       anyDuplicated(levels[!is.na(levels)])){
      cli_abort("Dataset {.val {dataset_name}} has invalid or duplicated {.field level} values.")
    }
    for(field in c("mean_n", "mean_rows")){
      values = suppressWarnings(as.numeric(rows[[field]]))
      if(any(!is.na(levels) & (is.na(values) | !is.finite(values) | values < 1))){
        cli_abort("Dataset {.val {dataset_name}} needs finite {.field {field}} values of at least one for its levels.")
      }
    }
    for(i in seq_len(nrow(rows))){
      for(field in c("depends_on", "unique")){
        value = as.character(rows[[field]][i])
        if(!is.na(value) && value != "" && !(field == "unique" && value == "*") &&
           !identical(.dummy_encode_values(.dummy_decode_values(value)), value)){
          cli_abort("Dataset {.val {dataset_name}}: {.field {field}} must contain an encoded column list, e.g. {.val 1:SUBJID}.")
        }
      }
      keys = .dummy_decode_values(rows$depends_on[i])
      partners = .dummy_decode_values(rows$unique[i])
      if(any(!c(keys, partners) %in% rows$column) || rows$column[i] %in% c(keys, partners)){
        cli_abort("Dataset {.val {dataset_name}}: {.field {rows$column[i]}} refers to missing columns or itself.")
      }
      if(!is.na(levels[i])){
        parent_levels = levels[match(keys, rows$column)]
        if(any(is.na(parent_levels) | parent_levels >= levels[i])){
          cli_abort("Dataset {.val {dataset_name}}: level dependencies must refer to earlier levels.")
        }
      }
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
