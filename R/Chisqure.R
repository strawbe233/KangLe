
#' @Title  batch chisquire test
#' @description  this function is used to batch implement chisqure test
#' @param data
#' @param x_variables
#' @param y_variable
#' @param output_file
#'
#' @return results
#' @export
#'
#' @examples
chisq_batch <- function(data, x_variables, y_variable, output_file) {
  library(openxlsx)
  results <- data.frame(Variable = character(), Chi_square = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

  for (variable in x_variables) {
    chisq_result <- chisq.test(data[[y_variable]]~data[[variable]])
    result <- data.frame(Variable = variable, Chi_square = chisq_result$statistic, p_value = chisq_result$p.value)
    results <- rbind(results, result)
  }

  write.xlsx(results, file = output_file, rownames = FALSE)
  return(results)
}
