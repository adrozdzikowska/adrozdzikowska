### -------------------------------    Function repair_mistakes()     ------------------------------

#' @title Function repair_mistakes
#' @param dt data.table with columns: ID, AGE, SEX
#'
#' @return dt with removed ID with AGE above 110 years ago and with SEX not in Male or Female
#' @export
#'
#' @examples
repair_mistakes <- function(dt)
{
  cat("Check column names\n")
  
  if (!all(c("ID", "AGE", "SEX") %in% colnames(dt))) {
    brakuje = paste(setdiff(c("ID", "AGE", "SEX"), colnames(dt)), collapse = ", ")
    stop(paste("Missing columns: \n", brakuje, "\nStop working, change your columns."))
  }
  
  # Check ID to remove
  if (nrow(dt[AGE >= 110 | !SEX %in% c("Male", "Female")]))
  {
    message(paste("Removed", nrow(dt[AGE > 110 | !SEX %in% c("Male", "Female")]), 
                  "rows from", 
                  uniqueN(dt[AGE > 110 | !SEX %in% c("Male", "Female")]), 
                  "unique ID, where AGE>110 or SEX not in Male, Female. This is", 
                  round(nrow(dt[AGE > 110 | !SEX %in% c("Male", "Female")])/nrow(dt)*100, digits = 3), "% rows from source data."))
    
    dt <- dt[as.numeric(AGE) <= 110]
    dt <- dt[SEX %in% c("Male", "Female")]
    dt <- dt[!ID %in% bad_age$ID]
  } 
  
  return(dt)
}

# # Use:
# dt_example1 = data.table("ID" = c("1","1","2"),
#                          "AGE" = c(110, 110,2),
#                          "SEX" = c("Masle", "Male", "Female"))
# 
# repair_mistakes(dt = dt_example1)



### -------------------------------    Function add_gAGE()     ------------------------------

#' @title Function add_gAGE
#' @param dt data.table with columns: AGE
#' @param limits boundaries of age groups
#' @param name name of column with age groups
#'
#' @return dt with added column with age groups
#' @export
#'
#' @examples
add_gAGE <- function(dt, 
                     limits = c(18, 40, 65), 
                     name = "gAGE")
{
  
  if (length(limits) > 1)
  {
    create_labels <- vapply(1:(length(limits) - 1),function(x){paste(limits[x], limits[x + 1]-1, sep = '-')}, '')
    create_labels = c(paste0('<', limits[1]), create_labels, paste0(limits[length(limits)], '+'))
  } else {
    create_labels <- c(paste0('<', limits[1]), paste0(limits[length(limits)], '+'))
  }
  
  dt[, (name) := as.character(cut(x = AGE, breaks = c(-Inf, limits, Inf), labels = create_labels, right = F))]
  
  return(dt)
}

# # Use:
# dt_example2 = data.table("ID" = c("1","1","2"),
#                          "AGE" = c(110, 110,2),
#                          "SEX" = c("Masle", "Male", "Female"))
# 
# add_gAGE(dt = dt_example2)
