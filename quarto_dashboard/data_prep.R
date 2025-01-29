source("config.R")

# READ IN THE DATA
GHG_ALL <- read_excel("../FlatOutput_DAGHGI_1990-2022.xlsx") %>%
  mutate(
    EmissionYear = case_when(
      EmissionYear == "BaseYear" ~ "1989", 
      TRUE ~ EmissionYear
    ),
    EmissionYear = as.numeric(EmissionYear)
  )


# TIDY THE DATA UP

# Intermediate result to preserve additional cols pre-group
GHG_ALL <- GHG_ALL %>%
  select(-Timestamp) %>%
  filter(ConvertTo == "GWP CO2_AR5" & TESS1 != "International aviation and shipping")

GHG_PROJ <- read_excel("../GHG Projections Data.xlsx")
PROJ_ACTUAL <- GHG_ALL %>%
  group_by(Pollutant, SourceName, EmissionYear, RegionName, IPCC_code, NCFormat) %>%
  summarise(Emission = sum(Emission), .groups = 'drop') %>%
  mutate(Mt = Emission / 1000)
regions <- c("Northern Ireland", "England", "Scotland", "Wales", "Unallocated")
frames <- c("PROJ_ACTUAL_NI", "PROJ_ACTUAL_ENG", "PROJ_ACTUAL_SCOT", "PROJ_ACTUAL_WAL", "PROJ_ACTUAL_UNALL")

for (i in 1:length(regions)) {
  assign(frames[i], filter(PROJ_ACTUAL, RegionName == regions[i]))
}

# Summarise keeping necessary grouped variables
GHG_ALL <- GHG_ALL %>%
  group_by(Pollutant, SourceName, EmissionYear, RegionName, IPCC_code, TESS1, TESS2, TESS3, NCFormat) %>%
  summarise(Emission = sum(Emission), .groups = 'drop') %>%
  mutate(Mt = Emission / 1000)

print(colnames(GHG_ALL))
# Filter data by region for performance
regions <- c("Northern Ireland", "England", "Scotland", "Wales", "Unallocated")
frames <- c("GHG_NI", "GHG_ENG", "GHG_SCOT", "GHG_WAL", "GHG_UNALL")

for (i in 1:length(regions)) {
  assign(frames[i], filter(GHG_ALL, RegionName == regions[i]))
}

# Define current year and other shared variables early
currentYear <- max(GHG_NI$EmissionYear)
years <- unique(GHG_NI$EmissionYear)

sectorColours <- data.frame(
  TESS1 = sort(unique(GHG_NI$TESS1)),
  colour = c("#197e84", "#831d5c", "#7e749d", "#819dcb",
             "#ee706f", "#009b3e", "#b5bc47", "#043c54")
)
# Helper functions
wrap.it <- function(x, len) {
  sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"), USE.NAMES = FALSE)
}

wrap.labels <- function(x, len) {
  if (is.list(x)) {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

sectortotdf <- GHG_NI %>%
  filter(EmissionYear == currentYear) %>%
  group_by(TESS1) %>%
  summarise(CO2 = sum(`Emission`))%>%
  mutate(sectorLabel = paste0(TESS1, ": ", prettyNum(round(CO2), big.mark = ","), " KtCO2e"),
         CO2 = round(CO2, 2))