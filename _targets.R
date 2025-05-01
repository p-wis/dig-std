library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble", "haven") # Packages that your targets need for their tasks.

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
    name = df,
    command = read_dta(data_fname)
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
    )
  )
)
