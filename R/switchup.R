#' Switch the first pair of values before and after '=' on the current line
#'
#' @export
switchup <- function() {

  doc <- rstudioapi::getActiveDocumentContext()
  doc_lines <- doc$contents
  doc_cursor_line <- rstudioapi::primary_selection(doc)$range$start[[1]]
  line_text <- doc_lines[doc_cursor_line]
  match <- regexpr("=", line_text)
  has_comma <- grepl(",$", line_text)

    if (match != -1) {

      n_whitespace_chars <- regexpr("\\S", line_text) - 2
      before <- trimws(substr(line_text, 1, match - 1))
      after <- trimws(substr(line_text, match + 1, nchar(line_text)))
      if (has_comma) {
        before = paste0(before, ",")
        after <- gsub(",", "", after)
      }
      if (nchar(before) > 0 & nchar(after) > 0) {
        new_text <- paste(
          paste(rep(" ", n_whitespace_chars), collapse = ""),
          after, '=', before
          )
        location <- c(doc_cursor_line, match + 1)
        rstudioapi::modifyRange(location = c(doc_cursor_line, 1, doc_cursor_line, Inf), text = "")
        rstudioapi::insertText(location = location, text = new_text)
      }
    } else {
      cat("No '=' found in the current line.\n")
    }
  }
