# Identidad de usuario: información de presentación, no autorización ----
.template_groups <- function(groups) {
  if (is.null(groups) || !length(groups)) return("ANALÍTICA")
  groups <- trimws(as.character(groups))
  groups <- groups[!is.na(groups) & nzchar(groups)]
  if (!length(groups)) return("ANALÍTICA")
  paste(unique(stringr::str_to_upper(groups)), collapse = " · ")
}
