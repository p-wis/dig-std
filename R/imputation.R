impute_df_trtmt <- function(df_trtmt, exclude_from_predictors) {
 
  ini <- mice(df_trtmt, maxit = 0)
  
  method <- ini$method
  method["reason"] <- ""  # exclude from being imputed
  
  pred <- ini$predictorMatrix
  
  pred[, exclude_from_predictors] <- 0
  
  imp <- mice(df_trtmt, m = 5, method = method,  predictorMatrix = pred, seed = 123)
  
  complete(imp, action = 1) 
  
}