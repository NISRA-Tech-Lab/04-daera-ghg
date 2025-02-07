# Click Source button or press Ctrl + Shift +S

# Libraries
library(quarto)
library(here)

# Run to render website
quarto_render(input = paste0(here(), "/pages"),
              as_job = FALSE)

# Run after render to fix date slider
html_docs <- list.files(paste0(here(), "/docs"), pattern = "*.html")

for (doc in html_docs) {
  
  suppressWarnings({
    raw_html <- readLines(paste0(here(), "/docs/", doc))
  })
  
  if (length(which(grepl("strftime", raw_html))) > 0) {
    strftime_line <- which(grepl("strftime", raw_html))
    selectize_line <- which(grepl('<link href="site_libs/selectize', raw_html))
    
    new_html <- c(raw_html[1:(selectize_line - 1)],
                  raw_html[strftime_line:(strftime_line + 1)],
                  raw_html[(selectize_line):(strftime_line - 1)],
                  raw_html[(strftime_line + 2):length(raw_html)])
    
    writeLines(new_html,
               paste0(here(), "/docs/", doc))
    
  }
  
}

# After render is complete run this to see output in browser
browseURL(paste0(here(), "/docs/index.html"))
