#' Create a portable dummy-data specification
#'
#' `edc_dummy_spec()` profiles the structure of an `edc_database` into a flat,
#' CSV-compatible data frame. The specification is intended to be editable and
#' transportable without the original patient-level database.
#'
#' The specification preserves column structure, classes, labels, coarse
#' generation parameters, and missingness. It also records observed uniqueness,
#' functional dependencies, and an inferred hierarchy of repeated observations.
#' Subject and date columns are considered first; remaining column order
#' guides the hierarchy.
#' These observed relationships are editable heuristics, not clinical rules.
#'
#' - `unique` is `*` for global uniqueness, or an encoded list of columns:
#'   each listed column separately forms a unique pair with this column.
#'   All supported column pairs are checked on complete observations.
#' - `depends_on` is an encoded list forming one grouping combination. A
#'   non-level column is generated once per group, then repeated.
#'   Dependencies are checked against successive hierarchy prefixes and
#'   earlier non-level columns. Missingness counts as a distinct value.
#' - `level` orders the inferred hierarchy. Level columns introduce distinct
#'   values within their parent group.
#' - `mean_n` is the mean number of child groups per parent; `mean_rows` is
#'   the mean number of rows per group. Both are rounded to one decimal.
#'
#' Column lists use the same encoding as categorical values, for example
#' `1:SUBJID` or `2:SUBJID|DATE`. Names containing separators are URL-encoded.
#' The structure fields can be edited or cleared to remove inferred constraints.
#'
#' Real subject identifiers, character values, and calendar dates are not copied
#' into the specification. Factor levels are treated as structural metadata and
#' are currently preserved. `SUBJID` is harmonized across datasets: if all
#' non-missing source identifiers can be interpreted as finite numbers, the
#' dummy identifiers are integer, even for character values such as `"001"`.
#' Otherwise they are character. Differing source classes or numeric
#' compatibility trigger a warning with dataset examples. If distinct IDs in
#' one dataset become identical after numeric conversion, the function stops
#' until `subjid_collision` is set to `"merge"` or `"split"`.
#'
#' @param db An `edc_database`.
#' @param subjid_collision What to do when distinct `SUBJID` values in one
#'   dataset become identical after numeric conversion (for example `"001"`
#'   and `"1"`). `NA` (default) stops with an error; `"merge"` treats them as
#'   one subject; `"split"` keeps them distinct and generates character IDs.
#'
#' @return A plain `data.frame` that can be written to CSV and read back with
#'   [utils::write.csv()] and [utils::read.csv()].
#' @export
edc_dummy_spec = function(db, subjid_collision = NA){
  if(!inherits(db, "edc_database")){
    cli_abort("{.arg db} must be an {.cls edc_database}.")
  }
  if(length(subjid_collision) != 1 ||
     !(is.na(subjid_collision) ||
       (is.character(subjid_collision) && subjid_collision %in% c("merge", "split")))){
    cli_abort("{.arg subjid_collision} must be {.val NA}, {.val merge}, or {.val split}.")
  }

  dataset_names = names(db)[vapply(db, is.data.frame, logical(1))]
  dataset_names = dataset_names[dataset_names != ".lookup"]
  if(length(dataset_names) == 0){
    cli_abort("{.arg db} does not contain any dataset.")
  }

  id_sources = lapply(dataset_names, function(dataset_name){
    data = db[[dataset_name]]
    columns = names(data)[toupper(names(data)) == "SUBJID"]
    if(length(columns) == 0) return(NULL)
    data.frame(
      dataset = dataset_name,
      class = vapply(columns, function(column){
        paste(class(data[[column]]), collapse = "|")
      }, character(1)),
      numeric = vapply(columns, function(column){
        .dummy_numeric_identifier(data[[column]])
      }, logical(1)),
      collision = vapply(columns, function(column){
        ids = data[[column]]
        if(!.dummy_numeric_identifier(ids)) return(FALSE)
        ids = unique(ids[!is.na(ids)])
        anyDuplicated(suppressWarnings(as.numeric(ids))) > 0
      }, logical(1)),
      stringsAsFactors = FALSE
    )
  })
  id_sources = Filter(Negate(is.null), id_sources)
  id_sources = if(length(id_sources) == 0) NULL else do.call(rbind, id_sources)

  if(!is.null(id_sources) && any(id_sources$collision) && is.na(subjid_collision)){
    datasets = unique(id_sources$dataset[id_sources$collision])
    cli_abort(c(
      "Distinct {.field SUBJID} values become identical after numeric conversion in: {.val {datasets}}.",
      i = "For example, {.val 001} and {.val 1} can represent different subjects.",
      i = "Choose {.code subjid_collision = 'merge'} for one subject or {.code subjid_collision = 'split'} to keep them distinct."
    ), class = "edc_dummy_subjid_collision_error")
  }

  id_class = if(is.null(id_sources)) NULL else if(all(id_sources$numeric) &&
    !(any(id_sources$collision) && identical(subjid_collision, "split"))) "integer" else "character"

  if(!is.null(id_sources) &&
     (length(unique(id_sources$class)) > 1 || length(unique(id_sources$numeric)) > 1)){
    examples = id_sources[!duplicated(id_sources[c("class", "numeric")]), , drop = FALSE]
    examples = paste0(
      examples$dataset, " (", examples$class, ", ",
      ifelse(examples$numeric, "numeric-compatible", "not numeric-compatible"), ")",
      collapse = ", "
    )
    explanation = if(any(id_sources$collision) && identical(subjid_collision, "split")){
      'Colliding identifiers remain distinct; dummy `SUBJID` will be character.'
    } else if(id_class == "integer"){
      'All non-missing identifiers are numeric-compatible (e.g. "001" and 1); dummy `SUBJID` will be integer.'
    } else {
      'Some identifiers are not numeric-compatible; dummy `SUBJID` will be character.'
    }
    cli_warn(c(
      "`SUBJID` differs across datasets, e.g. {examples}.",
      i = "{explanation}"
    ), class = "edc_dummy_subjid_class_warning")
  }

  specs = lapply(dataset_names, function(dataset_name){
    data = db[[dataset_name]]
    structure_data = data
    id_index = which(toupper(names(data)) == "SUBJID")
    n_subjects = nrow(data)
    if(length(id_index) > 0){
      ids = data[[id_index[1]]]
      if(id_class == "integer" ||
         (identical(subjid_collision, "merge") &&
          any(id_sources$collision[id_sources$dataset == dataset_name]))){
        ids = suppressWarnings(as.numeric(ids))
      }
      n_subjects = length(unique(ids[!is.na(ids)]))
      if(nrow(data) > 0 && n_subjects == 0) n_subjects = 1L
      structure_data[[id_index[1]]] = ids
    }
    structure = .dummy_profile_structure(structure_data)

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
        class = if(profile$generator == "identifier") id_class else paste(class(x), collapse = "|"),
        generator = profile$generator,
        unique = structure$unique[match(column_name, structure$column)],
        depends_on = structure$depends_on[match(column_name, structure$column)],
        level = structure$level[match(column_name, structure$column)],
        mean_n = structure$mean_n[match(column_name, structure$column)],
        mean_rows = structure$mean_rows[match(column_name, structure$column)],
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


.dummy_profile_structure = function(data){
  columns = names(data)
  rtn = data.frame(
    column = columns, unique = NA_character_, depends_on = NA_character_,
    level = NA_integer_, mean_n = NA_real_, mean_rows = NA_real_,
    stringsAsFactors = FALSE
  )
  if(nrow(data) == 0) return(rtn)
  supported = vapply(data, function(x){
    is.atomic(x) && (is.character(x) || is.numeric(x) || is.logical(x) ||
      is.factor(x) || inherits(x, "Date"))
  }, logical(1))
  eligible = columns[supported]
  global = vapply(eligible, function(column){
    x = data[[column]]
    x = x[!is.na(x)]
    length(x) > 1 && !anyDuplicated(x)
  }, logical(1))
  partners = setNames(vector("list", length(columns)), columns)
  if(length(eligible) > 1){
    pairs = combn(eligible, 2, simplify = FALSE)
    for(pair in pairs){
      observed = data[complete.cases(data[pair]), pair, drop = FALSE]
      if(nrow(observed) > 1 && !anyDuplicated(observed)){
        partners[[pair[1]]] = c(partners[[pair[1]]], pair[2])
        partners[[pair[2]]] = c(partners[[pair[2]]], pair[1])
      }
    }
  }
  for(column in eligible){
    i = match(column, columns)
    if(global[[column]]){
      rtn$unique[i] = "*"
    } else if(length(partners[[column]]) > 0){
      rtn$unique[i] = .dummy_encode_values(partners[[column]])
    }
  }

  id = eligible[toupper(eligible) == "SUBJID"]
  hierarchy = character()
  processed = character()
  parent_n = 1L
  if(length(id) > 0){
    hierarchy = id[1]
    processed = id
    parent_n = nrow(unique(data[hierarchy]))
    i = match(id[1], columns)
    rtn$level[i] = 1L
    rtn$mean_n[i] = parent_n
    rtn$mean_rows[i] = round(nrow(data) / parent_n, 1)
  }
  dates = eligible[vapply(data[eligible], inherits, logical(1), what = "Date")]
  ordered = unique(c(dates, eligible))
  for(column in setdiff(ordered, processed)){
    i = match(column, columns)
    candidates = lapply(seq_along(hierarchy), function(k) hierarchy[seq_len(k)])
    other = processed[!processed %in% hierarchy]
    candidates = c(candidates, lapply(other[!global[other]], function(x) x))
    dependency = NULL
    for(keys in candidates){
      if(.dummy_is_dependency(data, keys, column)){
        dependency = keys
        break
      }
    }
    if(!is.null(dependency)){
      rtn$depends_on[i] = .dummy_encode_values(dependency)
    } else {
      child_n = nrow(unique(data[c(hierarchy, column)]))
      if(child_n > parent_n){
        if(length(hierarchy) > 0){
          rtn$depends_on[i] = .dummy_encode_values(hierarchy)
        }
        rtn$level[i] = length(hierarchy) + 1L
        rtn$mean_n[i] = round(child_n / parent_n, 1)
        rtn$mean_rows[i] = round(nrow(data) / child_n, 1)
        hierarchy = c(hierarchy, column)
        parent_n = child_n
      }
    }
    processed = c(processed, column)
  }
  rtn
}


.dummy_is_dependency = function(data, keys, column){
  observed = data[c(keys, column)]
  if(all(is.na(observed[[column]]))) return(FALSE)
  nrow(unique(observed)) == nrow(unique(observed[keys]))
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
