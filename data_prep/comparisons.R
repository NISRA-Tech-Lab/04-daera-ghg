library(here)
source(paste0(here(), "/data_prep/main.R"))

#Create 2019 Bar Chart for all sectors.
homebardf <- GHG_NI %>%
  filter(EmissionYear == currentYear) %>%
  group_by(TESS1 ) %>%
  summarise(Emission = round(sum(`Emission`), 0)) %>%
  left_join(sectorColours) %>%
  mutate(TESS1  = wrap.labels(TESS1 , 12))

stackedBarData <- GHG_NI %>%
  group_by(Sector = TESS1 , eYear = EmissionYear) %>%
  summarise(Total_CO2e = round(sum(`Emission`))) %>%
  mutate(Sector = wrap.labels(Sector, 25),
         eYear = factor(eYear,
                        levels = 1989:max(eYear),
                        labels = c("Base\nyear", 1990:max(eYear)))) %>%
  pivot_wider(id_cols = eYear, names_from = Sector, values_from = Total_CO2e)




sectorSumAll <- GHG_ALL %>%
  filter(RegionName != "Unallocated") %>%
  group_by(EmissionYear, RegionName) %>%
  summarise(sumCO2 = sum(`Emission`)) %>%
  mutate(Year = factor(EmissionYear,
                       levels = years,
                       labels = c("Base year", years[2:length(years)]))) %>%
  pivot_wider(id_cols = Year, names_from = RegionName, values_from = sumCO2)

sectorSumNI <- GHG_NI %>%
  group_by(Year = EmissionYear, TESS1 ) %>%
  summarise(sumCO2 = sum(`Emission`)) %>%
  mutate(Sector = wrap.labels(TESS1 , 25),
         Year = factor(Year,
                       levels = years,
                       labels = c("Base year", years[2:length(years)]))) %>%
  pivot_wider(id_cols = Year, names_from = Sector, values_from = sumCO2)

