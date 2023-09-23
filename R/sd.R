
#' @Title  Batch sd
#' @description  this function is used to implement calculating SD
#' @param data
#' @param y_variable
#' @param x_variables
#' @param output_file
#'
#' @return
#' @export
#'
#' @examples
calculate_sd <- function(data, y_variable, x_variables, output_file) {
  library(openxlsx)
  result <- data.frame(Variable = character(), SD = double(), stringsAsFactors = FALSE)
  for (x_var in x_variables) {
    sd_results <- aggregate(data[[y_variable]] ~ data[[x_var]], data = data, FUN = sd, na.rm = TRUE)
    temp_var <- as.character(sd_results[[1]])
    temp_sd <- sd_results[[2]]
    result <- rbind(result, data.frame(Variable = temp_var, SD = temp_sd))
  }
  write.xlsx(result, file = output_file, rownames = FALSE)
}
