# Source libraries
library(quarto)
library(here)

# Update for name of folder
folder_name <- "quarto_dashboard"

# Run to render website
quarto_render(folder_name)

# After render is complete run this to see output in browser
browseURL(paste0(here(), "/", folder_name, "/_site/index.html"))
