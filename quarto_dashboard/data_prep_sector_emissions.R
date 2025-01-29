source("data_prep.R")

# Northern Ireland ####

sectordata1 <- GHG_ALL %>%
  select(-c(SourceName, IPCC_code, TESS2, Mt)) %>%
  group_by(EmissionYear, Pollutant, RegionName, TESS1) %>%
  summarise(Emission = sum(Emission))

sectordata2 <- GHG_ALL %>%
  group_by(EmissionYear, RegionName, TESS1) %>%
  summarise(Emission = sum(Emission))

sectordata2$Pollutant <- "(All Aggregated)"

sectordata3 <- GHG_ALL %>%
  group_by(EmissionYear, RegionName) %>%
  summarise(Emission = sum(Emission))

sectordata3$Pollutant <- "(All Aggregated)"
sectordata3$NCFormat <- "(All Aggregated)"

sectordata4 <- full_join(sectordata1, sectordata2)
sectordata4 <- full_join(sectordata4, sectordata3)%>%
  filter(RegionName != "Unallocated") %>%
  mutate(Emission = round(Emission, digits = 2),
         pre1997 = ifelse(EmissionYear > 1989 & EmissionYear < 1997, Emission, NA),
         post1997 = ifelse(EmissionYear > 1997 & EmissionYear <= currentYear, Emission, NA),
         label = ifelse(EmissionYear == 1989, TESS1, NA),
         eYear = factor(EmissionYear,
                        levels = 1989:currentYear,
                        labels = c("Base year", 1990:currentYear)),
         CO2comma = prettyNum(Emission, big.mark = ","),
         EmissionYear = as.Date(paste0(EmissionYear, "-01-01"))) %>%
  select(eYear, Pollutant, RegionName, TESS1, CO2comma, pre1997, post1997, EmissionYear, Emission) %>%
  group_by(Pollutant, RegionName, TESS1)

eYear2Labels <- c()
for (i in 2:length(levels(sectordata4$eYear))) {
  eYear2Labels[i] <- if (levels(sectordata4$eYear)[i] %in% levels(sectordata4$eYear)[as.numeric(levels(sectordata4$eYear)) %% 10 == 0]) {
    levels(sectordata4$eYear)[i]
  } else {
    paste0("'", substr(levels(sectordata4$eYear)[i], 3, 4))
  }
}

eYear2 <- factor(sectordata4$eYear,
                 levels = levels(sectordata4$eYear),
                 labels = eYear2Labels)

Shared_sectordata4 <- SharedData$new(sectordata4)
sectordata4$Pollutant <- "(All Aggregated)"

# Function for sectors ####

sectorEmission <- function(sector) {
  
  data1 <- GHG_NI %>%
    filter(TESS1 == sector) %>%
    select(-c(RegionName, TESS1, Mt))
  
  data2 <- GHG_NI %>%
    filter(TESS1 == sector) %>%
    group_by(EmissionYear, TESS2) %>%
    summarise (Emission = round(sum(Emission), 2), .groups = 'drop') %>%
    mutate (Pollutant = "(All Aggregated)",
            IPCC_code = "(All Aggregated)",
            TESS3 = "(All Aggregated)")
  
  data3a <- GHG_NI %>%
    filter(TESS1 == sector) %>%
    group_by(EmissionYear, TESS2,TESS3, Pollutant) %>%
    summarise(Emission = sum(Emission), .groups = 'drop') %>%
    mutate(IPCC_code = "(All Aggregated)")
  
  data3b <- GHG_NI %>%
    filter(TESS1 == sector) %>%
    group_by(EmissionYear, TESS2,TESS3, IPCC_code) %>%
    summarise(Emission = sum(Emission), .groups = 'drop') %>%
    mutate(Pollutant = "(All Aggregated)")
  
  data3c <- GHG_NI %>%
    filter(TESS1 == sector) %>%
    group_by(EmissionYear, TESS2, IPCC_code, Pollutant) %>%
    summarise(Emission = sum(Emission), .groups = 'drop') %>%
    mutate(TESS3 = "(All Aggregated)"
    )
  
  data3d <- GHG_NI %>%
    filter(TESS1 == sector) %>%
    group_by(EmissionYear, TESS2, IPCC_code) %>%
    summarise(Emission = sum(Emission), .groups = 'drop') %>%
    mutate(TESS3 = "(All Aggregated)", 
           Pollutant = "(All Aggregated)")
  
  data3e <- GHG_NI %>%
    filter(TESS1 == sector) %>%
    group_by(EmissionYear, TESS2, Pollutant) %>%
    summarise(Emission = sum(Emission), .groups = 'drop') %>%
    mutate(TESS3 = "(All Aggregated)", 
           IPCC_code = "(All Aggregated)")
  
  data3f <- GHG_NI %>%
    filter(TESS1 == sector) %>%
    group_by(EmissionYear, TESS2,TESS3) %>%
    summarise(Emission = sum(Emission), .groups = 'drop') %>%
    mutate(Pollutant = "(All Aggregated)", 
           IPCC_code = "(All Aggregated)")
  
  data4 <- full_join(data1, data2)
  
  data4 <- full_join(data4, data3a)
  
  data4 <- full_join(data4, data3b)
  
  data4 <- full_join(data4, data3c)
  
  data4 <- full_join(data4, data3d)
  
  data4 <- full_join(data4, data3e)
  
  data4 <- full_join(data4, data3f) %>%
    mutate(Emission = round(Emission, digits = 2),
           pre1997 = ifelse(EmissionYear > 1989 & EmissionYear < 1997, Emission, NA),
           post1997 = ifelse(EmissionYear > 1997, Emission, NA),
           eYear = factor(
             EmissionYear, 
             levels = 1989:currentYear, 
             labels = c("Base year", 1990:currentYear)),
           CO2comma = prettyNum(Emission, big.mark = ","),
           EmissionYear = as.Date(paste0(EmissionYear, "-01-01"))
    ) %>%
    select(eYear, Pollutant, TESS2,TESS3, IPCC_code, CO2comma, pre1997, post1997, EmissionYear, Emission) %>%
    group_by(Pollutant, 
             IPCC_code, 
             TESS2,
             TESS3)
  
  eYear2Labels <- c()
  for (i in 2:length(levels(data4$eYear))) {
    eYear2Labels[i] <- if (levels(data4$eYear)[i] %in% levels(data4$eYear)[as.numeric(levels(data4$eYear)) %% 10 == 0]) {
      levels(data4$eYear)[i]
    } else {
      paste0("'", substr(levels(data4$eYear)[i], 3, 4))
    }
  }
  
  eYear2 <- factor(data4$eYear, 
                   levels = levels(data4$eYear), 
                   labels = eYear2Labels)
  
  Shared_data4 <- SharedData$new(data4)
  
  lineText = paste("Sub-Sector:", data4$TESS2, "<br>Pollutant:", data4$Pollutant, "<br>IPCC_code:", data4$IPCC_code, "<br>Pollutant Source:", data4$TESS3, "<br>Emission:", data4$Emission)
  
  yaxistitle <- list(x = 0, y = 1, text = "CO2 Equivalents\n(Kilotonnes CO\u2082e)", 
                     showarrow = F, xref = "paper", yref = "paper", 
                     xanchor = "center", yanchor = "bottom",
                     font = list(size = 12))
  
  fig <- 
    plot_ly(Shared_data4, 
            x = eYear2, 
            y = ~post1997, 
            color = ~TESS2,  # Display TESS2 values only
            type = "scatter", 
            mode = "lines+markers",
            text = lineText, 
            hoverinfo = 'text', 
            showlegend = FALSE) %>%
    add_trace(y = ~pre1997, 
              mode = 'markers') %>%
    layout(xaxis = list(title = "",
                        tickangle = 0,
                        zeroline = FALSE),
           yaxis = list(title = "",
                        tickformat = ".2f",
                        showline = TRUE),
           annotations = list(yaxistitle),
           margin = list(t = 50),
           
           title = paste("Emissions in Northern Ireland", sector, "sector"),
           titlefont = list(size = 25),
           clickmode = "none") %>%
    config(displaylogo = FALSE)
  
  
  
  sectTable <- datatable(Shared_data4,
                          colnames = c("Year", "Pollutant", "Sub-sector", "Pollutant Source", "IPCC_code", "Kilotonnes CO\u2082e", "CO2 Equivalents (pre-1997) (kilotonnes CO2 e)", "CO2 Equivalents (post-1997) (kilotonnes CO2 e)", "[Emission Year]", "CO2 Equivalents. (kilotonnes CO2 e)"),
                          fillContainer = FALSE,
                          rownames = FALSE,
                          extensions = 'Buttons',
                          options = list(
                            columnDefs = list(
                              list(visible = FALSE, targets = 6:9),
                              list(className = "dt-body-right", targets = c(0, 5)),
                              list(className = "dt-right", targets = c(0, 5)),
                              list(className = "dt-left", targets = "_all")),
                            pageLength = 15,
                            lengthMenu = c(5, 10, 15, 20),
                            dom = 'Bfrtip',
                            buttons = list(
                              list(extend = "pdf", exportOptions = list(columns = ":visible")),
                              list(extend = "excel", exportOptions = list(columns = ":visible")),
                              list(extend = "csv", exportOptions = list(columns = ":visible")),
                              list(extend = "copy", exportOptions = list(columns = ":visible")),
                              list(extend = "print", exportOptions = list(columns = ":visible"))
                            )
                          ))
  
  # Define the filters
  filters <- bscols(
    list(""),
    list("Click inside boxes to add other lines, by region, pollutant, or sector. (All) represents the total of all gas types added together. Click below to add other gas types to the line graph and delete (All Aggregated) to remove it from the line graph",
         filter_select(id = "POLLUTANT", label = "Pollutant",sharedData = Shared_data4, group = ~Pollutant, selected = "(All Aggregated)"),
         "To focus on a particular TESS2, click and delete to remove from the line graph",
         filter_select(id = "TESS2", label = "TESS2", sharedData = Shared_data4, group = ~TESS2, selected = "(All Aggregated)"),
         "To focus on a particular TESS3, click and delete to remove from the line graph",
         filter_select(id = "TESS3", label = "TESS3", sharedData = Shared_data4, group = ~TESS3, selected = "(All Aggregated)"),  # New filter for TESS3
         "To focus on a particular IPCC code, click and delete to remove from the line graph",
         filter_select(id = "IPCC_code", label = "IPCC", sharedData = Shared_data4, group = ~IPCC_code, selected = "(All Aggregated)"),
         #  filter_select(id = "SourceName", label = "Pollutant Source", sharedData = Shared_data4, group = ~SourceName),
         filter_slider(id = "EmissionYear", label = "Year", sharedData = Shared_data4, column = ~EmissionYear, timeFormat = "%Y")
    ),
    list(""),
    widths = c(1, NA, 1)
  )
  
  list(fig = fig,
       sectTable = sectTable,
       filters = filters)
  
}
