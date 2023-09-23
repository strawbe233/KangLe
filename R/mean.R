#' @Title  Batch mean
#' @description  this function is used to batch implement calculating mean
#' @param data
#' @param y_variable
#' @param x_variables
#' @param output_file
#'
#' @return
#' @export
#'
#' @examples
calculate_mean <- function(data, y_variable, x_variables, output_file) {
  library(openxlsx)
  result <- data.frame(Variable = character(), Mean = double(), stringsAsFactors = FALSE)
  for (x_var in x_variables) {
    mean_results <- aggregate(data[[y_variable]] ~ data[[x_var]], data = data, FUN = mean, na.rm = TRUE)
    temp_var <- as.character(mean_results[[1]])
    temp_mean <- mean_results[[2]]
    result <- rbind(result, data.frame(Variable = temp_var, Mean = temp_mean))
  }
  write.xlsx(result, file = output_file, rownames = FALSE)
}
