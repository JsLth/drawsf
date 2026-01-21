case_switch <- function(..., .default = NULL) {
  env <- parent.frame()
  formulas <- list(...)
  for (fm in formulas) {
    cond <- as.logical(eval(fm[[2]], env = env))
    value <- fm[[3]]

    if (is.na(cond)) {
      cond_expr <- deparse(fm[[2]])
      cli::cli_warn("Expression `{cond_expr}` does not evaluate to a logical.")
      next
    }

    if (isTRUE(cond)) {
      return(eval(value, env = env))
    }
  }

  .default
}
