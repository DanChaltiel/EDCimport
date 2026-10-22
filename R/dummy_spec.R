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
#' - `singleton_prop` is the proportion of groups with one row, rounded to
#'   one decimal. It preserves small groups that a mean alone would remove.
#'
#' Column lists use the same encoding as categorical values, for example
#' `1:SUBJID` or `2:SUBJID|DATE`. Names containing separators are URL-encoded.
#' The structure fields can be edited or cleared to remove inferred constraints.
#' Character columns containing a uniform valid date format (`YYYY/MM`,
#' `YYYY-MM`, `MM/YYYY`, `MM-YYYY`, `YYYY/MM/DD`, or `YYYY-MM-DD`) use
#' `date_character` generation.
#' Their class and precision are preserved. `param3` records the format;
#' `param1` and `param2` give an artificial start and a coarse span in days.
#' Numeric columns containing only finite whole values within R's integer
#' range use the `integer` generator while retaining their source class.
#' Character columns use categorical generation when they have at most ten
#' distinct values, or at least five non-missing observations per distinct
#' value on average. For dependent columns, observations are counted once
#' per dependency group. Source character values are replaced by artificial
#' category names and the column remains character.
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
    formats = id_sources %>%
      group_by(across(all_of(c("class", "numeric")))) %>%
      summarise(datasets = paste(unique(.data$dataset), collapse = ", "), .groups = "drop")
    details = paste0(
      "SUBJID is ", formats$class, " (",
      ifelse(formats$numeric, "numeric-compatible", "not numeric-compatible"),
      ") in datasets: ", formats$datasets, "."
    )
    explanation = if(any(id_sources$collision) && identical(subjid_collision, "split")){
      'Colliding identifiers remain distinct; dummy `SUBJID` will be character.'
    } else if(id_class == "integer"){
      'All non-missing identifiers are numeric-compatible (e.g. "001" and 1); dummy `SUBJID` will be integer.'
    } else {
      'Some identifiers are not numeric-compatible; dummy `SUBJID` will be character.'
    }
    cli_warn(c(
      "`SUBJID` differs across datasets.",
      set_names(paste0("{details[", seq_along(details), "]}"), rep("i", length(details))),
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
    profile_cache = new.env(parent = emptyenv())

    rows = lapply(names(data), function(column_name){
      x = data[[column_name]]
      profile_x = x
      index = match(column_name, structure$column)
      dependency = structure$depends_on[index]
      if(is.character(x) && is.na(structure$level[index]) && !is.na(dependency)){
        if(!exists(dependency, envir = profile_cache, inherits = FALSE)){
          keys = .dummy_decode_values(dependency)
          groups = .dummy_group_ids(structure_data, keys)
          assign(dependency, which(!duplicated(groups)), envir = profile_cache)
        }
        profile_x = x[get(dependency, envir = profile_cache, inherits = FALSE)]
      }
      profile = .dummy_profile_column(column_name, profile_x)
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
        singleton_prop = structure$singleton_prop[match(column_name, structure$column)],
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
    level = NA_integer_, mean_n = NA_real_, mean_rows = NA_real_, singleton_prop = NA_real_,
    stringsAsFactors = FALSE
  )
  if(nrow(data) == 0) return(rtn)
  supported = vapply(data, function(x){
    is.atomic(x) && (is.character(x) || is.numeric(x) || is.logical(x) ||
      is.factor(x) || inherits(x, "Date"))
  }, logical(1))
  eligible = columns[supported]
  encoded = lapply(data[eligible], function(x) match(x, unique(x)))
  known = lapply(data[eligible], function(x) !is.na(x))
  cardinality = vapply(eligible, function(column){
    length(unique(encoded[[column]][known[[column]]]))
  }, integer(1))
  group_cache = new.env(parent = emptyenv())
  groups = function(keys){
    if(length(keys) == 1) return(encoded[[keys]])
    cache_key = paste(match(keys, eligible), collapse = ",")
    if(!exists(cache_key, envir = group_cache, inherits = FALSE)){
      parent = groups(head(keys, -1L))
      pairs = .dummy_integer_pair(parent, encoded[[tail(keys, 1L)]])
      assign(cache_key, match(pairs, unique(pairs)), envir = group_cache)
    }
    get(cache_key, envir = group_cache, inherits = FALSE)
  }
  global = vapply(eligible, function(column){
    n = sum(known[[column]])
    n > 1 && cardinality[[column]] == n
  }, logical(1))
  partners = set_names(vector("list", length(columns)), columns)
  if(length(eligible) > 1){
    pairs = utils::combn(eligible, 2, simplify = FALSE)
    for(pair in pairs){
      observed = known[[pair[1]]] & known[[pair[2]]]
      n = sum(observed)
      if(n <= 1 || as.double(cardinality[[pair[1]]]) * cardinality[[pair[2]]] < n) next
      is_unique = any(global[pair]) ||
        !anyDuplicated(.dummy_integer_pair(encoded[[pair[1]]][observed], encoded[[pair[2]]][observed]))
      if(is_unique){
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
    parent_n = max(groups(hierarchy))
    i = match(id[1], columns)
    rtn$level[i] = 1L
    rtn$mean_n[i] = parent_n
    rtn$mean_rows[i] = round(nrow(data) / parent_n, 1)
    rtn$singleton_prop[i] = round(mean(tabulate(groups(hierarchy)) == 1L), 1)
  }
  dates = eligible[vapply(data[eligible], function(x){
    inherits(x, "Date") || !is.na(.dummy_character_date_format(x))
  }, logical(1))]
  ordered = unique(c(dates, eligible))
  for(column in setdiff(ordered, processed)){
    i = match(column, columns)
    candidates = lapply(seq_along(hierarchy), function(k) hierarchy[seq_len(k)])
    other = processed[!processed %in% hierarchy]
    candidates = c(candidates, lapply(other[!global[other]], function(x) x))
    dependency = NULL
    for(keys in candidates){
      if(!any(known[[column]])) break
      group = groups(keys)
      values = encoded[[column]]
      if(all(values == values[match(group, group)])){
        dependency = keys
        break
      }
    }
    if(!is.null(dependency)){
      rtn$depends_on[i] = .dummy_encode_values(dependency)
    } else {
      child_n = max(groups(c(hierarchy, column)))
      if(child_n > parent_n){
        if(length(hierarchy) > 0){
          rtn$depends_on[i] = .dummy_encode_values(hierarchy)
        }
        rtn$level[i] = length(hierarchy) + 1L
        rtn$mean_n[i] = round(child_n / parent_n, 1)
        rtn$mean_rows[i] = round(nrow(data) / child_n, 1)
        hierarchy = c(hierarchy, column)
        rtn$singleton_prop[i] = round(mean(tabulate(groups(hierarchy)) == 1L), 1)
        parent_n = child_n
      }
    }
    processed = c(processed, column)
  }
  rtn
}


.dummy_integer_pair = function(x, y){
  if(length(x) == 0) return(numeric())
  width = max(x)
  if(as.double(width) * max(y) <= 2^53){
    x + (as.double(y) - 1) * width
  } else paste(x, y, sep = ":")
}


.dummy_is_dependency = function(data, keys, column){
  observed = data[c(keys, column)]
  if(all(is.na(observed[[column]]))) return(FALSE)
  nrow(unique(observed)) == nrow(unique(observed[keys]))
}


.dummy_character_date_format = function(x){
  if(!is.character(x)) return(NA_character_)
  values = unique(x[!is.na(x)])
  if(length(values) == 0) return(NA_character_)
  patterns = c(
    "%Y/%m" = "^[0-9]{4}/(0[1-9]|1[0-2])$",
    "%Y-%m" = "^[0-9]{4}-(0[1-9]|1[0-2])$",
    "%m/%Y" = "^(0[1-9]|1[0-2])/[0-9]{4}$",
    "%m-%Y" = "^(0[1-9]|1[0-2])-[0-9]{4}$",
    "%Y/%m/%d" = "^[0-9]{4}/(0[1-9]|1[0-2])/[0-9]{2}$",
    "%Y-%m-%d" = "^[0-9]{4}-(0[1-9]|1[0-2])-[0-9]{2}$"
  )
  for(date_format in names(patterns)){
    if(!all(grepl(patterns[[date_format]], values))) next
    partial = !grepl("%d", date_format, fixed = TRUE)
    separator = if(grepl("/", date_format, fixed = TRUE)) "/" else "-"
    complete = if(partial) paste0(values, separator, "01") else values
    full_format = if(partial) paste0(date_format, separator, "%d") else date_format
    parsed = suppressWarnings(as.Date(complete, format = full_format))
    if(!anyNA(parsed) && all(format(parsed, date_format) == values)) return(date_format)
  }
  NA_character_
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

  date_format = .dummy_character_date_format(x)
  if(!is.na(date_format)){
    n_values = length(unique(x[!is.na(x)]))
    step = if(grepl("%d", date_format, fixed = TRUE)) 1L else 31L
    span = max(365, 365 * ceiling((n_values - 1) * step / 365))
    return(list(
      generator = "date_character",
      param1 = "2000-01-01",
      param2 = as.character(span),
      param3 = date_format
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
    whole = length(y) > 0 && all(is.finite(y)) && all(y == trunc(y)) &&
      all(y >= -.Machine$integer.max & y <= .Machine$integer.max)
    return(list(
      generator = if(whole) "integer" else "numeric",
      param1 = as.character(bounds[1]),
      param2 = as.character(bounds[2]),
      param3 = NA_character_
    ))
  }

  if(is.character(x)){
    y = x[!is.na(x)]
    n_values = length(unique(y))
    if(n_values <= 10 || n_values * 5 <= length(y)){
      categories = paste0("category_", seq_len(max(1, n_values)))
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
