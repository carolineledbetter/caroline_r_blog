data(GermanCredit, package = "caret")
tibble::glimpse(GermanCredit)
fct_dummy <- function(data, 
                            variables = tidyselect::everything(), 
                            sep = '.') {
  variables <- rlang::enquo(variables)
  # transform to long format the dummy columns
  tmp <- 
    tidyr::pivot_longer(data, 
                        cols = intersect(tidyselect::contains(sep),  
                                           !!variables),
                        names_to = c("groups", "levels"),
                        names_pattern = paste0("^([^'", sep, "]*)[", 
                                               sep, "](.*)"))
  
  # get the groups name for column selection after
  groups <- unique(tmp$groups)
  
  
  # keep only non dummy value and do not keep temp value col
  tmp <- dplyr::select(
    dplyr::filter(tmp, value == 1),
    -value)
  
  # function to return 'multiple' if more than 1 value is present
  ret_multiple <- function(x){
    if(length(x) > 1) return('multiple')
    return(x)
  }
  
  
  # tranform to wide format   
  tmp <- tidyr::pivot_wider(
    tmp,
    names_from = groups, 
    values_from = levels, 
    values_fn = list(levels = ret_multiple))
  
  
  # convert to factors the groups column
  dplyr::mutate_at(
    tmp,
    groups,
    ~ forcats::as_factor(.)
  )
}

  
 
new_dat <- dummy_to_factor(GermanCredit)
tibble::glimpse(new_dat)

dat <- dplyr::rename_all(GermanCredit, stringr::str_replace, pattern = '[.]', replacement = '_')
dplyr::glimpse(dummy_to_factor(dat, sep = '_', tidyselect::starts_with('P')))

