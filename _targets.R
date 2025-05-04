library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble", "dplyr", "haven") # Packages that your targets need for their tasks.

 )

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

list(
    tar_target(
      name = data_fname,
      command = "1_dig-1.dta",
      format = "file"
    ),
  
    tar_target(
      name = zm_kategoryczne,
      command = c("sex", "race", "nsym", "functcls", "chfetiol", "prevmi", "angina",
          "diabetes", "hyperten", "diguse", "diuretk", "diuret", "ksupp", "aceinhib",
          "nitrates", "hydral", "vasod", "any_diuret", "any_vasod")
    ),
    
    
  tar_target(
    name = df,
    command = {
      raw_df <- read_dta(data_fname)
      
      # Add derived variables
  
      raw_df <- raw_df %>%
        mutate(
          any_diuret = case_when(
            diuretk == 1L ~ 1L,
            diuret == 1L ~ 1L,
            TRUE ~ 0L
          ),
          any_vasod = case_when(
            nitrates == 1L ~ 1L,
            hydral == 1L ~ 1L,
            vasod == 1L ~ 1L,
            TRUE ~ 0L
          )
        )
      
      attr(raw_df[["any_diuret"]], "label") <- "derived: positive diuretk or diuret"
      
      attr(raw_df[["any_vasod"]], "label") <- "derived: positive nitrates or hydral or vasod"
      
      # Extract variable labels
      var_labels <- lapply(raw_df, function(x) attr(x, "label"))
      
      
      # Convert selected variables to factors
      df <- raw_df %>%
        mutate(across(all_of(zm_kategoryczne), haven::as_factor))
      
      # Restore variable labels
      for (v in names(var_labels)) {
        if (!is.null(var_labels[[v]])) {
          attr(df[[v]], "label") <- var_labels[[v]]
        }
      }
  
        
      df
    }
  ),
  
  tar_target(
    name = df_trtmt,
    command = dplyr::filter(df, trtmt == 1)
  ),
  
  tar_target(
    name = lista_zmiennych,
    command = tibble(
      var_labels = sapply(df, function(x) attr(x, "label"), USE.NAMES = TRUE),
      var_names = names(var_labels) 
    ) %>% 
    select(
      var_names, var_labels
    )
  )
  
 
)
