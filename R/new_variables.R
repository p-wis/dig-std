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