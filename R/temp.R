#' @importFrom sjmisc var_rename
#' @importFrom nlme intervals
#' @importFrom dplyr select
#' @importFrom tibble rownames_to_column
tidy_gls_model <- function(model, ci.lvl) {
  # get tidy summary. for lme, this excludes CI,
  # so we compute them separately

  dat <- as.data.frame(summary(model)$tTable)
  ci <- as.data.frame(nlme::intervals(model, level = ci.lvl, which = "coef")$coef)

  dat$conf.low <- ci$lower
  dat$conf.high <- ci$upper


  sjmisc::var_rename(
    dat,
    Value = "estimate",
    Std.Error = "std.error",
    `t-value` = "statistic",
    `p-value` = "p.value"
  ) %>%
    tibble::rownames_to_column("term")
}

