## Project Overview

This project facilitates the creation, management, and publication of a Quarto-based website for the *Northern Ireland Greenhouse Gas Inventory 1990-2022*. It includes automated rendering, dependency management, and structured content updates to ensure a seamless workflow.

### Prerequisites

- **R 4.4**, **RStudio 2024.04.1-748**, and **GIT for Windows** are required. All are available from ITAssist Store on your desktop.
- All required R packages are listed in `config.R`. By correctly initializing and restoring `renv`, all necessary dependencies will be installed automatically.

### Project Structure

1. **Website Generation**
   - The project uses Quarto to render `.qmd` files into a structured website.
   - The generated files are stored in the `/docs` directory.

2. **Automated Fixes**
   - The script corrects an issue with the date slider in the rendered HTML files.
   - If an issue with `strftime` is detected, affected lines are reordered to ensure proper functionality.

3. **Navigation & Styling**
   - The website is configured through `_quarto.yml`, defining:
     - Website title
     - Navigation bar with sector-specific emission pages
     - Custom favicon and styles

### Using `renv` for Dependency Management

This project includes the `renv` package to manage dependencies. The `.Rprofile` file ensures that the appropriate repositories are set up, and `renv` is activated upon loading the project.

#### First-Time Setup
1. Open R in the project directory.
2. Run `renv::restore()` to install the required packages from the lockfile.
3. Ensure that binary package installations are preferred for efficiency (`renv.config.install.prefer.binary = TRUE`).

#### Repository Configuration
The `.Rprofile` script dynamically configures package repositories based on available shared drives and includes a fallback to CRAN. This ensures that the correct versions of required packages are sourced from internal or public repositories.

### How to Update

To update the project with new data or adjustments, follow these steps:

1. **Update Data Filenames**
   - Modify the filenames in `config.R` to reflect the latest datasets:
     ```r
     GHG_ALL_name <- "FlatOutput_DAGHGI_1990-2022.xlsx"
     GHG_PROJ_name <- "GHG Projections Data.xlsx"
     ```
   - Ensure that the new data files are available in the expected locations.

2. **Update Website Title**
   - Open `_quarto.yml` and update the `title` field to reflect the new reporting period or version:
     ```yaml
     title: "Northern Ireland Greenhouse Gas Inventory 1990-2022"
     ```
   - Adjust other metadata as needed to align with the latest changes.

3. **Render the Dashboard**
   - Open the `render_dashboard.R` script.
   - Click **Source** or press `Ctrl + Shift + S` to run the script and generate the updated website.

### Rendering a Single Page

- To render a single page, open the relevant `.qmd` file in RStudio and click the **Render** button.
- If the page contains a date slider, you will need to run the full `render_dashboard.R` script afterward to apply the necessary bug fix.

### Publishing the Website

- To publish the website, send the entire rendered contents of the `/docs` folder to **NISRA TechLab** for hosting on the `explore.nisra.gov.uk` server.

### How to Use

1. Open R and ensure that `renv` is initialized and restored.
2. Run the script to render the website and apply the fix.
3. After execution, the website will automatically open in your default web browser.

### Notes

- The script should be executed in an R environment where Quarto is properly configured.
- The fix for the date slider is applied only if the issue is detected in the rendered HTML files.
- The project configuration ensures that the website maintains a structured navigation and consistent styling.
- Ensure that `renv` is initialized and restored to maintain package version consistency.

This project ensures that the *Northern Ireland Greenhouse Gas Inventory* website is correctly rendered, updated, and published without requiring manual intervention.

