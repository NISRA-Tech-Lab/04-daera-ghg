# Source libraries
library(quarto)
library(here)

# Run to render website
quarto_render(input = paste0(here(), "/pages"),
              as_job = FALSE)

# Run after render to fix date slider
html_docs <- list.files(paste0(here(), "/docs"), pattern = "*.html")

for (doc in html_docs) {
  
  raw_html <- readLines(paste0(here(), "/docs/", doc))
  
  if (length(which(grepl("strftime", raw_html))) > 0) {
    strftime_line <- which(grepl("strftime", raw_html))
    jquery_line <- which(grepl('<script src="site_libs/jquery', raw_html))
    
    new_html <- c(raw_html[1:(jquery_line + 1)],
                  raw_html[strftime_line:(strftime_line + 1)],
                  raw_html[(jquery_line + 2):(strftime_line - 1)],
                  raw_html[(strftime_line + 2):length(raw_html)])
    
    writeLines(new_html,
               paste0(here(), "/docs/", doc))
    
  }
  
}

# After render is complete run this to see output in browser
browseURL(paste0(here(), "/docs/index.html"))
