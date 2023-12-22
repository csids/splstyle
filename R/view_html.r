#' View HTML in the rstudio view
#' @param x html
#' @returns Nothing
#' @export
view_html <- function(x){
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")
  # (code to write some content to the file)
  htmlPage <- paste("<html>",
                    "<head>",
                    "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\">",
                    "</head>",
                    "<body>",
                    "<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
                    x,
                    "</div>",
                    "</body>",
                    "</html>", sep="\n")
  cat(htmlPage, file=htmlFile)
  #viewer <- getOption("viewer")
  rstudioapi::viewer(htmlFile)
}


