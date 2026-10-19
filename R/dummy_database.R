#' Generate a dummy EDC database from a specification
#'
#' `edc_dummy_database()` only uses the portable specification produced by
#' [edc_dummy_spec()]. It does not require access to the original database.
#'
#' The generated database uses artificial subject identifiers of a single type
#' across all datasets (`character` or `integer`) and a fixed artificial extraction
#' date. Its `.lookup`
#' table is built and extended from the dummy datasets by `new_edc_database()`.
#' Observed uniqueness, functional dependencies, and rounded repetition means
#' are respected when present in the specification. Values are generated once
#' at their inferred level and repeated on lower-level rows. Row counts and
#' missing proportions can therefore differ slightly from the source.
#' Missing groups are selected jointly with uniqueness constraints. If the
#' requested non-missing proportion cannot be reached, extra missing groups
#' are reported with a warning. A column specified without missingness still
#' raises an error rather than introducing missing values.
#' Finite domains of non-missing unique partners also limit how often a value
#' can be repeated, accounting for all rows represented by a generated group.
#' Rows are allocated to the first level before constructing child groups,
#' so rounding at successive levels does not compound within a subject.
#' This should not be interpreted as an anonymisation or synthetic-data method
#' preserving clinical or statistical relationships.
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
  spec$class = as.character(spec$class)
  id_rows = spec$generator == "identifier"
  if(any(id_rows)) spec$class[id_rows] = .dummy_identifier_class(spec$class[id_rows])

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
    extend_lookup = TRUE,
    set_lookup = TRUE,
    verbose = FALSE,
    dummy = TRUE
  )
  class(dummy) = c("edc_dummy", "edc_database")
  dummy
}


.dummy_generate_dataset = function(spec){
  spec = .dummy_structure_defaults(spec)
  spec$level = as.numeric(spec$level)
  spec$mean_n = as.numeric(spec$mean_n)
  spec$mean_rows = as.numeric(spec$mean_rows)
  n_rows = unique(as.integer(spec$n_rows))
  n_subjects = unique(as.integer(spec$n_subjects))
  layout = .dummy_generate_layout(spec, n_rows, n_subjects)
  n_rows = layout$n_rows
  columns = list()
  pending = seq_len(nrow(spec))
  while(length(pending) > 0){
    ready = pending[vapply(pending, function(i){
      all(.dummy_decode_values(spec$depends_on[i]) %in% names(columns))
    }, logical(1))]
    if(length(ready) == 0){
      cli_abort("Dataset {.val {spec$dataset[1]}} has circular dependencies.")
    }
    priority = ifelse(spec$generator[ready] == "identifier", 0L,
      ifelse(spec$generator[ready] == "date", 1L,
        ifelse(!is.na(spec$level[ready]), 2L, 3L)))
    i = ready[order(priority, spec$level[ready], na.last = TRUE)[1]]
    row = spec[i, , drop = FALSE]
    name = as.character(row$column)
    keys = .dummy_decode_values(row$depends_on)
    generated = as.data.frame(columns, check.names = FALSE)
    units = if(name %in% names(layout$codes)){
      layout$codes[[name]]
    } else if(length(keys) > 0){
      .dummy_group_ids(generated, keys)
    } else seq_len(n_rows)
    representatives = which(!duplicated(units))
    n_units = length(representatives)
    if(row$generator == "identifier"){
      x = .dummy_subject_ids(n_subjects, as.character(row$class))
      x = if(name %in% names(layout$codes)) x[units] else rep(x, length.out = n_rows)
    } else {
      scopes = list()
      forced_missing = integer()
      if(identical(as.character(row$unique), "*")){
        scopes = list(rep(1L, n_units))
        forced_missing = which(tabulate(match(units, units[representatives])) > 1L)
      } else {
        partners = intersect(.dummy_decode_values(row$unique), names(columns))
        for(partner in partners){
          values = generated[[partner]]
          pairs = data.frame(unit = units, value = values)
          repeated = !is.na(values) & duplicated(pairs)
          forced_missing = union(forced_missing, match(units[repeated], units[representatives]))
          scope = .dummy_group_ids(generated[representatives, , drop = FALSE], partner)
          scope[is.na(values[representatives])] = NA_integer_
          scopes = c(scopes, list(scope))
        }
      }
      if(!is.na(row$level) && length(keys) > 0){
        scopes = c(scopes, list(.dummy_group_ids(generated[representatives, , drop = FALSE], keys)))
      }
      weights = tabulate(match(units, units[representatives]), nbins = n_units)
      capacity = .dummy_partner_capacity(spec, row)
      x = .dummy_generate_missing(row, n_units, scopes, forced_missing, weights, capacity)
      x = x[match(units, units[representatives])]
    }
    label = .dummy_scalar_character(row$column_label)
    if(!is.na(label)) attr(x, "label") = label
    columns[[name]] = x
    pending = setdiff(pending, i)
  }
  data = as.data.frame(columns[as.character(spec$column)], stringsAsFactors = FALSE, check.names = FALSE)
  .dummy_check_structure(data, spec)
  dataset_label = .dummy_scalar_character(spec$dataset_label[1])
  if(!is.na(dataset_label)) attr(data, "label") = dataset_label
  data
}


.dummy_balanced_counts = function(n, groups){
  if(groups == 0) return(integer())
  counts = rep(n %/% groups, groups)
  remainder = n %% groups
  if(remainder > 0){
    i = sample.int(groups, remainder)
    counts[i] = counts[i] + 1L
  }
  counts
}


.dummy_generate_layout = function(spec, n_rows, n_subjects){
  levels = which(!is.na(spec$level))
  levels = levels[order(spec$level[levels])]
  if(length(levels) == 0 || n_rows == 0){
    return(list(n_rows = n_rows, codes = list()))
  }
  group_counts = integer(length(levels))
  n_parent = 1L
  for(k in seq_along(levels)){
    i = levels[k]
    row = spec[i, , drop = FALSE]
    n_groups = if(row$generator == "identifier") n_subjects else
      max(n_parent, min(n_rows, round(n_parent * as.numeric(row$mean_n))))
    if(row$generator != "identifier" && identical(as.character(row$unique), "*")) n_groups = n_rows
    group_counts[k] = n_groups
    n_parent = n_groups
  }
  n_rows = min(n_rows, max(n_parent, round(n_parent * as.numeric(spec$mean_rows[tail(levels, 1)]))))
  capacity = .dummy_partner_capacity(spec, spec[levels[1], , drop = FALSE])
  n_rows = min(n_rows, group_counts[1] * capacity)
  codes = list()
  for(k in seq_along(levels)){
    column = as.character(spec$column[levels[k]])
    if(k == 1){
      codes[[column]] = rep(seq_len(group_counts[k]), .dummy_balanced_counts(n_rows, group_counts[k]))
    } else {
      parents = split(seq_len(n_rows), codes[[k - 1L]])
      counts = .dummy_bounded_counts(group_counts[k], lengths(parents))
      child = integer(n_rows)
      offset = 0L
      for(j in seq_along(parents)){
        rows = parents[[j]]
        child[rows] = offset + rep(seq_len(counts[j]), .dummy_balanced_counts(length(rows), counts[j]))
        offset = offset + counts[j]
      }
      codes[[column]] = child
    }
  }
  list(n_rows = n_rows, codes = codes)
}


.dummy_bounded_counts = function(n, capacity){
  n = max(length(capacity), min(n, sum(capacity)))
  counts = rep(1L, length(capacity))
  remaining = n - sum(counts)
  while(remaining > 0){
    available = which(counts < capacity)
    if(remaining < length(available)){
      chosen = available[sample.int(length(available), remaining)]
      counts[chosen] = counts[chosen] + 1L
      remaining = 0L
    } else {
      additions = pmin(capacity[available] - counts[available], remaining %/% length(available))
      counts[available] = counts[available] + additions
      remaining = remaining - sum(additions)
    }
  }
  counts
}


.dummy_generate_values = function(row, n){
  class_string = as.character(row$class)
  param1 = .dummy_scalar_character(row$param1)
  param2 = .dummy_scalar_character(row$param2)
  switch(
    as.character(row$generator),
    categorical = .dummy_generate_categorical(n, class_string, param1),
    logical = runif(n) < .dummy_number(param1, 0.5),
    integer = .dummy_generate_integer(n, param1, param2),
    numeric = .dummy_generate_numeric(n, param1, param2),
    constant = .dummy_generate_constant(n, class_string, param1),
    date = .dummy_generate_date(n, param1, param2),
    text = if(n == 0) character() else paste("dummy text", seq_len(n)),
    unsupported = rep(NA, n),
    cli_abort("Unsupported dummy generator {.val {row$generator}}.")
  )
}


.dummy_value_domain = function(row){
  if(row$generator == "categorical"){
    values = .dummy_decode_values(row$param1)
    if(length(values) == 0) values = c("level_1", "level_2")
    if(.dummy_has_class(row$class, "factor")){
      values = factor(values, levels = values, ordered = .dummy_has_class(row$class, "ordered"))
    }
    return(values)
  }
  if(row$generator == "logical") return(c(FALSE, TRUE))
  if(row$generator == "constant") return(.dummy_generate_values(row, 1))
  if(row$generator == "date"){
    start = suppressWarnings(as.Date(row$param1))
    if(is.na(start)) start = as.Date("2000-01-01")
    span = max(0L, as.integer(.dummy_number(row$param2, 365)))
    if(span <= 1000000L) return(start + seq.int(0L, span))
  }
  if(row$generator == "integer"){
    bounds = sort(c(.dummy_number(row$param1, 0), .dummy_number(row$param2, 10)))
    lower = ceiling(bounds[1])
    upper = floor(bounds[2])
    if(lower > upper) lower = upper
    if(upper - lower <= 1000000L) return(as.integer(seq.int(lower, upper)))
  }
  if(row$generator == "numeric" && identical(row$param1, row$param2)){
    return(.dummy_generate_values(row, 1))
  }
  NULL
}


.dummy_generate_unique = function(row, n, scopes){
  x = .dummy_generate_values(row, n)
  if(n == 0 || length(scopes) == 0 || row$generator %in% c("text", "unsupported")) return(x)
  domain = .dummy_value_domain(row)
  scopes = .dummy_unique_scopes(scopes)
  if(length(scopes) == 0) return(x)
  for(scope in scopes){
    if(!is.null(domain) && max(tabulate(scope)) > length(domain)){
      cli_abort("Dataset {.val {row$dataset}}: {.field {row$column}} needs more distinct values than its generator allows. Edit its bounds or uniqueness constraints.")
    }
  }
  if(length(scopes) == 1 && !is.null(domain)){
    for(indices in split(seq_len(n), scopes[[1]])){
      x[indices] = domain[sample.int(length(domain), length(indices), replace = FALSE)]
    }
    return(x)
  }
  valid = vapply(scopes, function(scope){
    !anyDuplicated(data.frame(scope = scope, value = x))
  }, logical(1))
  if(all(valid)) return(x)
  for(i in seq_len(n)){
    previous = seq_len(i - 1L)
    blocked = unique(unlist(lapply(scopes, function(scope){
      previous[scope[previous] == scope[i]]
    })))
    if(length(blocked) == 0) next
    if(!is.null(domain)){
      available = which(!domain %in% x[blocked])
      if(length(available) == 0){
        cli_abort("Dataset {.val {row$dataset}}: cannot satisfy the combined uniqueness constraints for {.field {row$column}}. Edit the specification.")
      }
      x[i] = domain[available[sample.int(length(available), 1L)]]
    } else {
      attempts = 0L
      while(x[i] %in% x[blocked] && attempts < 100L){
        x[i] = .dummy_generate_values(row, 1L)
        attempts = attempts + 1L
      }
      if(x[i] %in% x[blocked]){
        cli_abort("Dataset {.val {row$dataset}}: cannot generate unique values for {.field {row$column}}. Edit the specification.")
      }
    }
  }
  x
}


.dummy_unique_scopes = function(scopes){
  scopes = unique(lapply(scopes, function(scope){
    missing = is.na(scope)
    scope = match(scope, unique(scope))
    if(any(missing)) scope[missing] = max(scope) + seq_len(sum(missing))
    scope
  }))
  scopes[vapply(scopes, function(scope) anyDuplicated(scope) > 0, logical(1))]
}


.dummy_partner_capacity = function(spec, row){
  partners = .dummy_decode_values(row$unique)
  capacities = vapply(seq_len(nrow(spec)), function(i){
    target = spec[i, , drop = FALSE]
    related = row$column %in% .dummy_decode_values(target$unique) || target$column %in% partners
    if(!related || as.numeric(target$missing_prop) > 0) return(Inf)
    domain = .dummy_value_domain(target)
    if(is.null(domain)) Inf else length(domain)
  }, numeric(1))
  min(capacities)
}


.dummy_generate_missing = function(row, n, scopes, forced_missing, weights, capacity){
  x = .dummy_generate_values(row, n)
  if(n == 0) return(x)
  x[] = NA
  target = n - round(n * as.numeric(row$missing_prop))
  candidates = setdiff(sample.int(n), union(forced_missing, which(weights > capacity)))
  scopes = .dummy_unique_scopes(scopes)
  domain = if(length(scopes) > 0 || is.finite(capacity)) .dummy_value_domain(row) else NULL
  selected = integer()
  if(is.null(domain)){
    selected = head(candidates, target)
    x[selected] = .dummy_generate_unique(row, length(selected), lapply(scopes, function(scope) scope[selected]))
  } else {
    used = lapply(scopes, function(scope) vector("list", max(scope)))
    frequency = numeric(length(domain))
    values = .dummy_generate_values(row, n)
    for(i in candidates){
      if(length(selected) >= target) break
      blocked = unique(unlist(lapply(seq_along(scopes), function(k){
        used[[k]][[scopes[[k]][i]]]
      })))
      available = which(!seq_along(domain) %in% blocked & frequency + weights[i] <= capacity)
      if(length(available) == 0) next
      value = match(values[i], domain)
      if(!value %in% available) value = available[sample.int(length(available), 1L)]
      x[i] = domain[value]
      frequency[value] = frequency[value] + weights[i]
      selected = c(selected, i)
      for(k in seq_along(scopes)){
        group = scopes[[k]][i]
        used[[k]][[group]] = c(used[[k]][[group]], value)
      }
    }
  }
  if(length(selected) < target){
    if(as.numeric(row$missing_prop) == 0){
      cli_abort("Dataset {.val {row$dataset}}: {.field {row$column}} cannot satisfy its uniqueness constraints without adding missing values. Edit its repetition structure or uniqueness constraints.")
    }
    cli_warn(c(
      "Dataset {.val {row$dataset}}: extra missing values were needed for {.field {row$column}} to respect uniqueness and dependencies.",
      i = "Generated {length(selected)} non-missing groups instead of the requested {target}."
    ), class = "edc_dummy_missingness_warning")
  }
  x
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
