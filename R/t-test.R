
#' @Title Batch t test
#' @description  This function is used to batch implementation of t tests
#' @param data
#' @param y_variable
#' @param x_variables
#' @param output_file
#'
#' @return
#' @export
#'
#' @examples
t_test_batch <- function(data, y_variable, x_variables, output_file) {
  library(openxlsx)
  results <- data.frame(Variable = character(), t_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

  for (variable in x_variables) {
    test_result <- t.test(data[[y_variable]] ~ data[[variable]])
    result <- data.frame(Variable = variable, t_value = test_result$statistic, p_value = test_result$p.value)
    results <- rbind(results, result)
  }

  write.xlsx(results, file = output_file, rownames = FALSE)
}
