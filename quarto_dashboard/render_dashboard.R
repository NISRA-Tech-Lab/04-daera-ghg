# Source libraries
library(quarto)
library(here)

# Update for name of folder
folder_name <- "quarto_dashboard"

# Run to render website
quarto_render(folder_name)


html_docs <- list.files(paste0(here(), "/", folder_name, "/_site"), pattern = "*.html")

for (doc in html_docs) {
  
  raw_html <- readLines(paste0(here(), "/", folder_name, "/_site/", doc))
  
  if (length(which(grepl("strftime", raw_html))) > 0) {
    strftime_line <- which(grepl("strftime", raw_html))
    jquery_line <- which(grepl('<script src="site_libs/jquery', raw_html))
    
    new_html <- c(raw_html[1:(jquery_line + 1)],
                  raw_html[strftime_line:(strftime_line + 1)],
                  raw_html[(jquery_line + 2):(strftime_line - 1)],
                  raw_html[(strftime_line + 2):length(raw_html)])
    
    writeLines(new_html,
               paste0(here(), "/", folder_name, "/_site/", doc))
    
  }
  
}

# After render is complete run this to see output in browser
browseURL(paste0(here(), "/", folder_name, "/_site/index.html"))


