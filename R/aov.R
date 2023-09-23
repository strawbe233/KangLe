#' @Title Batch aov
#' @description this function is used to batch implement aov test
#' @param data
#' @param y_variable
#' @param x_variables
#' @param output_file
#'
#' @return
#' @export
#'
#' @examples
aov_batch <- function(data, y_variable, x_variables, output_file) {
  library(openxlsx)
  results <- data.frame(Variable = character(), F_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

  for (variable in x_variables) {

    aov_result <- summary(aov(data[[y_variable]] ~ data[[variable]], data = data))
    result <- data.frame(Variable = variable, F_value = aov_result[[1]][[4]], p_value = aov_result[[1]][[5]])
    results <- rbind(results, result)
  }

  write.xlsx(results, file = output_file, rownames = FALSE)
}
