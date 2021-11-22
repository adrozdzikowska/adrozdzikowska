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
  } 
  
  return(dt)
}

# # Use:
# dt_example1 = data.table("ID" = c("1","1","2"),
#                          "AGE" = c(110, 111, 2),
#                          "SEX" = c("Male", "-", "Female"))
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
# dt_example2 = data.table("AGE" = c(5, 40, 110))
# 
# add_gAGE(dt = dt_example2)


### -------------------------------    Function create_pathways()     ------------------------------

#' @title Function create_pathways
#' @param dt data.table with columns: ID_PERSON, TYPE, DATE
#' @param simplify logical vector, when TURE it simplifies paths - groups multiple occurrences of the same type into one instance, default is FALSE
#'
#' @return patient paths (by unique ID_PERSON) in successive types (TYPE) over time (DATE)
#' @export
#'
#' @examples
create_pathways <- function(dt, 
                            simplify = FALSE)
{
  
  indispensable <- c("ID_PERSON", "TYPE", "DATE")
  tryCatch(stopifnot(all(indispensable %in% names(dt))),
           error = function(e) stop(paste0("Missing columns: ", 
                                           stringr::str_c(setdiff(indispensable, names(dt)), collapse = ", "))))
  
  tryCatch(stopifnot(inherits(dt$DATE, "Date")),
           error = function(e) stop("Column DATE must be class Date"))
  
  dt <- data.table(dt)
  dt_path <- dt[order(ID_PERSON, TYPE, DATE)]
  
  paths <- unique(dt_path[, ':=' (count = .N, min_date = min(DATE),
                                  path = stringr::str_c(TYPE, collapse = " -> ")), 
                          by = ID_PERSON][, -c("DATE", "TYPE")])[, nodes := stringr::str_split(path, " -> ")]
  
  if (simplify) {
    paths$nodes <- lapply(paths$nodes, function(x){x[x != shift(x, n = 1, type = "lag", fill = "")]})
    paths$path <- unlist(lapply(paths$nodes, function(x) {stringr::str_c(x, collapse = " -> ")}))
  }
  
  return(paths)
}

# # Use:
# dt_example3 = data.table("ID_PERSON" = c( 1, 1, 2),
#                          "TYPE" = c("a", "a", "c"),
#                          "DATE" = c(as.Date("2020-02-02"), as.Date("2021-02-03"), as.Date("2021-09-03")))
# 
# paths <- create_pathways(dt = dt_example3)
# paths
