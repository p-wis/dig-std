library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble", "dplyr", "haven", "rms", "mice") # Packages that your targets need for their tasks.

 )

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

# tar_source("other_functions.R") # Source other scripts as needed.

list(
    tar_target(
      name = data_fname,
      command = "1_dig-1.dta",
      format = "file"
    ),
  
    tar_target(
      name = zm_kategoryczne,
      command = c("sex", "race", "nsym", "functcls", "nyha_class", "chfetiol", "prevmi", "angina",
          "diabetes", "hyperten", "diguse", "diuretk", "diuret", "ksupp", "aceinhib",
          "nitrates", "hydral", "vasod", "any_diuret", "any_vasod", "ejf_35", "chestx_55")
    ),
    
    tar_target(
      name = outcome_varnames,
      command = c("digdose", "cvd", "cvddays", "whf", "whfdays", "dig", "digdays",
                  "mi", "midays", "uang", "uangdays", "strk", "strkdays", "sva", 
                  "svadays", "vena", "venadays", "crev", "crevdays", "ocvd",
                  "ocvddays", "rinf", "rinfdays", "oth", "othdays", "hosp", 
                  "hospdays", "nhosp", "death", "deathday", "reason", "dwhf", 
                  "dwhfdays")
    ),
    
    tar_target(
      name = custom_varnames,
      command = c("any_diuret", "any_vasod", "nyha_class", "meanbp", "ejf_35", "chestx_55")
    ),   
    
  tar_target(
    name = df,
    command = {
      raw_df <- read_dta(data_fname)
      
      # Add derived variables
  
      raw_df <- raw_df %>%
        add_any_diuret() %>%
        add_any_vasod() %>%
        add_nyha_class() %>%
        add_meanbp() %>%
        add_ejf_35() %>%
        add_chestx_55()
      
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
    command = {
      pre_imputed <- df %>%
        filter(trtmt == 1) %>%
        select(-all_of(custom_varnames))
        
      
      ini <- mice(pre_imputed, maxit = 0)
      
      method <- ini$method
      method["reason"] <- ""  # exclude from being imputed
      
      pred <- ini$predictorMatrix
      
      pred[, outcome_varnames] <- 0
      
      imp <- mice(pre_imputed, m = 5, method = method,  predictorMatrix = pred, seed = 123)
      imputed <- complete(imp, action = 1) 
      
      imputed <- imputed %>% 
        add_any_diuret() %>%
        add_any_vasod() %>%
        add_nyha_class() %>%
        add_meanbp() %>%
        add_ejf_35() %>%
        add_chestx_55()      
      
      imputed
    }
  ),
  
  tar_target(
    name = dd_df_trmnt,
    command = rms::datadist(df_trtmt)
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
