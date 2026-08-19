#' Insert a change log section to the active document
#' This function is not intended to be used in any scripts.
#' It is intended to be used with qmd files.
#' Place the cursor where you want the change log and run the addin.
#' @return adds a change log section to the active document
#' @export
add_ctu_header <- function(){
rstudioapi::insertText(
glue::glue(
"
# Change log

| Release date | Summary of changes & Reason for change(s) |
|--|--------|
|19.08.2026 | Initial version |
"))
}
