tibble::glimpse(GermanCredit)
dummy_to_factor <- function(data, 
                            variables = everything(), 
                            sep = '.') {
  variables <- rlang::enquo(variables)
  # get the variables names for included variables  
  data_names <- names(dplyr::select(data, !!variables))
  
  # create a names list that can be used in nest with the group and
  # the variables that are in that group
  groups <- 
    dplyr::tibble(var_names = 
                    data_names[dplyr::contains(sep, vars = data_names)])
  if(!all(dplyr::select(data, groups$var_names) == 0 |
          dplyr::select(data, groups$var_names) == 1)) {
    stop('All dummy values must be 0 or 1')
  }
  groups <- 
    dplyr::mutate(groups, 
                  group = stringr::str_remove(var_names, 
                                              paste0("[", sep, "].*$"))
    )
  groups <- 
    dplyr::group_by(groups, group)
  groups <- 
    tidyr::nest(groups, grouped_cols = var_names)
  groups <- 
    dplyr::mutate(groups, grouped_cols = purrr::map(grouped_cols, c))
  groups <- 
    tidyr::unnest(groups, cols = grouped_cols)
  groups <- 
    tibble::deframe(groups)
  
  # function for determining which column has a 1 and retrieving that column 
  # name (and drop the group name)
  convert <- function(x){
    if(sum(x) > 1) return('multiple')
    if(sum(x) <= 0) return(NA_character_)
    x <- dplyr::rename_all(x, stringr::str_remove, 
                           paste0('^[^', sep, ']*[', sep, ']'))
    x <- tidyr::pivot_longer(x, cols = everything(), 
                             names_to = 'V1', 
                             values_to = 'V2')
    x <- dplyr::filter(x, V2 == 1) 
    return(x$V1)
  }
  
  # nest the dummy groups and convert them to factors
  data <- dplyr::group_by(data, id = dplyr::row_number())
  data <- 
    tidyr::nest(data, !!!groups) 
  data <- dplyr::mutate_at(data, names(groups), purrr::map_chr, convert)
  data <- dplyr::ungroup(data)
  data <- dplyr::select(data, -id)
}
new_dat <- dummy_to_factor(GermanCredit[1:10, ])
tibble::glimpse(new_dat)
system.time(dummy_to_factor(GermanCredit[1:10, ]))
system.time(dummy_to_factor(GermanCredit[1:50, ]))
system.time(dummy_to_factor(GermanCredit))
