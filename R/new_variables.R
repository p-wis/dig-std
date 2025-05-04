add_any_diuret <- function(raw_df) {
  raw_df <- raw_df %>%
    mutate(
      any_diuret = case_when(
        diuretk == 1L ~ 1L,
        diuret == 1L ~ 1L,
        TRUE ~ 0L
      )
    )
      
  attr(raw_df[["any_diuret"]], "label") <- "derived: positive diuretk or diuret"
  
  return(raw_df)
}

add_any_vasod <- function(raw_df) {
  raw_df <- raw_df %>%
    mutate(
      any_vasod = case_when(
        nitrates == 1L ~ 1L,
        hydral == 1L ~ 1L,
        vasod == 1L ~ 1L,
        TRUE ~ 0L
    )
   )
  
  attr(raw_df[["any_vasod"]], "label") <- "derived: positive nitrates or hydral or vasod"
      
  return(raw_df)
}

add_nyha_class <- function(raw_df) {
  
  raw_df <- raw_df %>%
    mutate(
      nyha_class = case_when(
        functcls %in% c(1, 2) ~ 1L,
        functcls %in% c(3, 4) ~ 2L,
        is.na(functcls) ~ NA_integer_,
        TRUE ~ NA_integer_
      )
    )  
  
  
  attr(raw_df[["nyha_class"]], "label") <- "derived: nyha class 1=1-2, 2=3-4"
  
  return(raw_df)
}

add_meanbp <- function(raw_df) {
  
  raw_df <- raw_df %>%
    mutate(
      meanbp =  diabp + 1/3*(sysbp-diabp)
    )  
  
  attr(raw_df[["meanbp"]], "label") <- "derived: mean blood pressure"
  
  return(raw_df)
}