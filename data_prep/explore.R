library(here)
source(paste0(here(), "/data_prep/main.R"))

# CREATING RANKING TABLE WITH FILTER

GHG_NI_thisYr <- GHG_NI %>%
  filter(EmissionYear == currentYear) %>%
  group_by(Sector = TESS1 , `Pollutant Source` = SourceName) %>%
  summarise(CO2 = round(sum(`Emission`), 2)) %>%
  arrange(desc(CO2)) %>%
  mutate(CO2 = format(CO2, big.mark = ","))

names(GHG_NI_thisYr)[names(GHG_NI_thisYr) == "CO2"] <- "Total Emissions (KtCO\u2082e)"

sharedGHG_NI_thisYr <- SharedData$new(GHG_NI_thisYr, key =~Sector)

# Treemap

sectorLabels <- sectortotdf %>%
  select(TESS1 , sectorLabel)

alltreemapdf <- GHG_NI %>%
  filter(EmissionYear==currentYear) %>%
  group_by(TESS1, TESS2, TESS3) %>%
  summarise(Emission = sum(`Emission`)) %>%
  left_join(sectorLabels, by = "TESS1")


formatLabels <- alltreemapdf %>%
  group_by(TESS2, TESS3, TESS1) %>%
  summarise(count = n()) %>%
  mutate(subsector = case_when(
    TESS2 == lag(TESS3) ~ paste0(TESS2, " (",TESS1, ")"),
    TESS2 == lead(TESS3) ~ paste0(TESS2, " (", TESS1, ")"),
    TRUE ~ TESS2
  )) %>%
  select(-count)

alltreemapdf <- left_join(alltreemapdf, formatLabels, by = c("TESS1", "TESS2", "TESS3")) %>%
  mutate(subsector = wrap.labels(subsector, 15))


tmap_labels <- c("2022 Sector Emissions", unique(alltreemapdf$sectorLabel), alltreemapdf$TESS3) # Update the labels vector
tmap_parents <- c("", rep("2022 Sector Emissions", length(unique(alltreemapdf$TESS1))), alltreemapdf$sectorLabel)  # Update the parents vector
tmap_values <- c(rep(0, length(unique(alltreemapdf$TESS1)) + 1), round(alltreemapdf$Emission, 0)) 

# Projections

projection <- PROJ_ACTUAL_NI %>%
group_by(EmissionYear, NCFormat) %>%
  summarise(`Emission` = sum(`Emission`))

projection$Source <- "Actual"


projection2 <- GHG_PROJ %>%
  filter(Year > currentYear) %>%
  select(-c(GHG, Emissions)) %>%
  na.omit()

projection2$Source <- "Projection"
names(projection2)[names(projection2) == "Year"] <- "EmissionYear"
names(projection2)[names(projection2) == "Sector"] <- "NCFormat"
names(projection2)[names(projection2) == "Projections"] <- "Emission"

lastProj <- max(projection2$EmissionYear)

projection2$`Emission`<-as.numeric(projection2$`Emission`)

projectionFull <- full_join(projection, projection2)  %>%
  mutate(`Emission` = round(`Emission`, digits = 2),
         pre1997 = ifelse(EmissionYear < 1997, `Emission`, NA),
         post1997 = ifelse(EmissionYear > 1997 & EmissionYear <= currentYear, `Emission`, NA),
         projection = ifelse(EmissionYear > 2021, `Emission`, NA),
         label = ifelse(EmissionYear == 1989, NCFormat, NA),
         eYear = factor(EmissionYear,
                        levels = 1989:lastProj,
                        labels = c("Base year", 1990:lastProj)),
         CO2comma = prettyNum(`Emission`, big.mark = ","),
         EmissionYear = as.Date(paste0(EmissionYear, "-01-01"))) %>%
  select(eYear, NCFormat, Source, CO2comma, pre1997, post1997, EmissionYear, `Emission`, projection) %>%
  group_by(NCFormat)

eYear2Labels <- c()
for (i in 1:length(levels(projectionFull$eYear))) {
  eYear2Labels[i] <- if (levels(projectionFull$eYear)[i] == "Base year") {
    "Base\nyear"
  } else if (levels(projectionFull$eYear)[i] %in% levels(projectionFull$eYear)[as.numeric(levels(projectionFull$eYear)) %% 10 == 0]) {
    levels(projectionFull$eYear)[i]
  } else {
    paste0("'", substr(levels(projectionFull$eYear)[i], 3, 4))
  }
}

eYear2 <- factor(projectionFull$eYear,
                 levels = levels(projectionFull$eYear),
                 labels = eYear2Labels)

Shared_projection <- SharedData$new(projectionFull)

lineTextProj = paste("\nSector:", projectionFull$NCFormat)
