library(here)
source(paste0(here(), "/data_prep/main.R"))

#Aggregate by year, sector, PollutantSum
PollutantSum <- GHG_NI %>%
  filter(EmissionYear > 1989) %>%
  group_by(EmissionYear) %>%
  summarise(sumCO2 = sum(`Emission`)/1000, .gorups ='drop')

# Calculate CO2 only data
CO2only <- GHG_NI %>%
  filter(EmissionYear > 1989, Pollutant == "CO2") %>%
  group_by(EmissionYear) %>%
  summarise(sumCO2 = sum(Emission) / 1000, .groups = 'drop')

#Pre and post 1997 data
PollutantSum_split <- PollutantSum %>%
  mutate(pre1997 = ifelse(EmissionYear < 1997, sumCO2, NA),
         post1997 = ifelse(EmissionYear > 1997, sumCO2, NA))

CO2only_split <- CO2only %>%
  mutate(pre1997 = ifelse(EmissionYear < 1997, sumCO2, NA),
         post1997 = ifelse(EmissionYear > 1997, sumCO2, NA))

# Colours
green <- "#009b3e"
blue <- "#1c5d97"

# Load dashboard/images used for plotting infographic
greenCloud <- paste0(here(), "/pages/img/Green cloud.png")
blueCloud <- paste0(here(), "/pages/img/Blue cloud.png")
greenArrowDown <- paste0(here(), "/pages/img/Green arrow down.png")
blueArrowDown <- paste0(here(), "/pages/img/Blue arrow down.png")
greenArrowUp <- paste0(here(), "/pages/img/Green arrow up.png")
blueArrowUp <- paste0(here(), "/pages/img/Blue arrow up.png")

# Create data frame of values needed for infogrpahic
co2Compare <- GHG_NI %>%
  # Filter for This year, last year and 1990
  filter(EmissionYear %in% c(1990, currentYear-1, currentYear)) %>%
  # Group by year and then sum CO2 Equiv
  group_by(EmissionYear) %>%
  summarise(Emission = sum(Emission), .groups = 'drop') %>%
  # Repeat above process for just CO2 pollutant and join to data
  left_join(GHG_NI %>%
              filter(EmissionYear %in% c(1990, currentYear-1, currentYear) & Pollutant == "CO2") %>%
              group_by(EmissionYear) %>%
              summarise(CO2 = sum(Emission), .groups = 'drop')) %>%
  # Divide figures by 1,000 to obtain Megatonne value and round to 1 dp
  mutate(`Emission` = round(`Emission` / 1000, 1),
         CO2 = round(CO2 / 1000, 1))

# Calculate percentage change 
pctCO2Equiv <-
  round((co2Compare$`Emission`[co2Compare$EmissionYear == currentYear-1] -
           co2Compare$`Emission`[co2Compare$EmissionYear == currentYear]) /
          co2Compare$`Emission`[co2Compare$EmissionYear == currentYear-1] * 100)

# Calculate percentage change in Emission equivalent of all greenhouse gases from 1990
pctCO2Equiv_1990 <-
  round((co2Compare$`Emission`[co2Compare$EmissionYear == 1990] -
           co2Compare$`Emission`[co2Compare$EmissionYear == currentYear]) /
          co2Compare$`Emission`[co2Compare$EmissionYear == 1990] * 100)

# Calculate percentage change in CO2 from last year
pctCO2 <-
  round((co2Compare$CO2[co2Compare$EmissionYear == currentYear-1] -
           co2Compare$CO2[co2Compare$EmissionYear == currentYear]) /
          co2Compare$CO2[co2Compare$EmissionYear == currentYear-1] * 100)

# Calculate percentage change in CO2 from 1990
pctCO2_1990 <-
  round((co2Compare$CO2[co2Compare$EmissionYear == 1990] -
           co2Compare$CO2[co2Compare$EmissionYear == currentYear]) /
          co2Compare$CO2[co2Compare$EmissionYear == 1990] * 100)

# Determine arrows for change based on percentage changes
greenArrow1 <- if (pctCO2Equiv < 0) greenArrowUp else greenArrowDown
pctCO2Equiv <- abs(pctCO2Equiv)

greenArrow2 <- if (pctCO2Equiv_1990 < 0) greenArrowUp else greenArrowDown
pctCO2Equiv_1990 <- abs(pctCO2Equiv_1990)

blueArrow1 <- if (pctCO2 < 0) blueArrowUp else blueArrowDown
pctCO2 <- abs(pctCO2)

blueArrow2 <- if (pctCO2_1990 < 0) blueArrowUp else blueArrowDown
pctCO2_1990 <- abs(pctCO2_1990)

# Colours
green <- "#009b3e"
blue <- "#1c5d97"

# dashboard/images
greenCloudReverse <- paste0(here(), "/pages/img/Green cloud reverse.png")
blueCloudReverse <- paste0(here(), "/pages/img/Blue cloud reverse.png")
darkBlueCloud <- paste0(here(), "/pages/img/Dark Blue Cloud.png")
darkBlueCloudReverse <- paste0(here(), "/pages/img/Dark Blue Cloud reverse.png")
lightBlueCloud <- paste0(here(), "/pages/img/Light Blue Cloud.png")
lightBlueCloudReverse <- paste0(here(), "/pages/img/Light Blue Cloud reverse.png")

# Reshape data to obtain values for infographic
# First for NI
comp_NI <- GHG_NI %>%
  # Filter on current year
  filter(EmissionYear == currentYear) %>%
  # Group Pollutants into "Other" and sum
  mutate(Pollutant = case_when(Pollutant %in% c("CO2", "CH4", "N2O") ~ Pollutant,
                               TRUE ~ "Other")) %>%
  group_by(Pollutant) %>%
  summarise(`Emission` = sum(`Emission`)) %>%
  # Calculate percentage
  mutate(pct = paste0(round(`Emission` / sum(`Emission`) * 100), "%"))

# Repeat above for all regions
comp_All <- GHG_ALL %>%
  # Filter on current year
  filter(EmissionYear == currentYear) %>%
  # Group Pollutants into "Other" and sum
  mutate(Pollutant = case_when(Pollutant %in% c("CO2", "CH4", "N2O") ~ Pollutant,
                               TRUE ~ "Other")) %>%
  group_by(Pollutant) %>%
  summarise(`Emission` = sum(`Emission`)) %>%
  # Calculate percentage
  mutate(pct = paste0(round(`Emission`/sum(`Emission`) * 100), "%"))

# Obtain total figures for bottom of infographic
# NI figure available from cloud and arrow infographic 
co2EquivNI <- co2Compare$`Emission`[co2Compare$EmissionYear == currentYear]
# UK figure available by summing column from comp_All data frame above
co2EquivAll <- round(sum(comp_All$`Emission`) / 1000, 1)

# Colour code
lightBlue <- "#36a9e1"

# Set PfG Baseline Year
PfGBaselineYear <- 2019


# Calculate the current year emissions and the baseline year emissions in Megatons and creates a data frame
pfG <- GHG_NI %>%
  filter(EmissionYear %in% c(currentYear, PfGBaselineYear)) %>%
  group_by(EmissionYear) %>%
  summarise(Emission = round(sum(Emission) / 1000, 1), .groups = 'drop')

# Calculate the difference in Emissions from the Baseline Year to the Current Year
pfGChange <- pfG$Emission[pfG$EmissionYear == currentYear] - pfG$Emission[pfG$EmissionYear == PfGBaselineYear]
# Calculate the percentage difference in Emissions from the Baseline Year to the Current Year
pfGChangePercent <- (pfGChange / pfG$`Emission`[pfG$EmissionYear == PfGBaselineYear]) * 100
# Calculate the level of significance to determine if an actual change has occurred between the baseline and current year
pfGSigChange <- currentYear - PfGBaselineYear

# IF statement to determine the background image and accompanying text depending on the percentage change compared with the required level of significance
if(PfGBaselineYear == currentYear) {
  pfgInfo <- paste0(here(), "/pages/img/PfgNoChange.png")
  pfgText1 <- ""
  pfgText2 <- ""
} else if(pfGChangePercent >= (pfGSigChange * -1) & pfGChangePercent <= pfGSigChange) {
  pfgInfo <- paste0(here(), "/pages/img/PfgNoChange.png")
  pfgText1 <- "No"
  pfgText2 <- "change"
} else if (pfGChangePercent < (pfGSigChange * -1)) {
  pfgInfo <- paste0(here(), "/pages/img/PfgNoChange.png")
  pfgText1 <- ""
  pfgText2 <- ""
} else {
  pfgInfo <- paste0(here(), "/pages/img/PfgIncreased.png")
  pfgText1 <- ""
  pfgText2 <- "Increased"
}

# Create dataframe for homepage treemap. 
hometreemapdf <- GHG_NI %>%
  filter(EmissionYear == currentYear) %>%
  mutate(TESS1  = case_when(TESS1 %in% "Fuel supply" ~ "Other",
                            TESS1 == "LULUCF" ~ "Land Use",
                            TESS1 == "Waste" ~ "Waste Mgmt", TRUE ~ TESS1))%>%
  group_by(TESS1) %>%
  summarise(CO2_total = sum(`Emission`)) %>%
  left_join(sectorColours) %>%
  mutate(percentage = paste0(round(CO2_total / sum(CO2_total) * 100), "%"),
         CO2_total = round(CO2_total),
         colour = case_when(TESS1 == "Land Use" ~ sectorColours$colour[sectorColours$TESS1 == "LULUCF"],
                            TESS1 == "Waste Mgmt" ~ sectorColours$colour[sectorColours$TESS1 == "Waste"],
                            TESS1 == "Other" ~ sectorColours$colour[sectorColours$TESS1 == "Fuel supply"],
                            TRUE ~ colour))

sectorChange <- GHG_NI %>%
  filter(EmissionYear %in% c(1989, currentYear)) %>%
  mutate(TESS1  = case_when(TESS1  == "Land use, land use change and forestry" ~ "Land Use",
                            TRUE ~ TESS1))%>%
  group_by(`<b>Sector</b>` = TESS1, EmissionYear) %>%
  filter(TESS1 != "Fuel supply") %>% # Filter fuel supply from chart
  summarise(co2Total = sum(`Emission`)) %>%
  ungroup() %>%
  pivot_wider(id_cols = `<b>Sector</b>`,
              names_from = EmissionYear,
              names_glue = "<b>{EmissionYear}</b>",
              values_from = co2Total,
              values_fn = function(x) {prettyNum(round(x), big.mark = ",")}) %>%
  mutate(`<b>Change</b>` = round(((as.numeric(sub(",","",.[[3]])) - as.numeric(sub(",","",.[[2]]))) / as.numeric(sub(",","",.[[2]])) * 100)),
         textColour = case_when(`<b>Change</b>` > 0 ~ "red",
                                `<b>Change</b>` < 0 ~ "green",
                                TRUE ~ "black"),
         ` ` = case_when(`<b>Change</b>` > 0 ~ "\u2B06",
                         `<b>Change</b>` < 0 ~ "\u2B07",
                         TRUE ~ ""),
         `<b>Change</b>` = case_when(`<b>Change</b>` > 0 ~ paste0("<b>+", `<b>Change</b>`, "%</b>"),
                                     TRUE ~ paste0("<b>", `<b>Change</b>`, "%</b>")))

colnames(sectorChange)[2]  <- "<b>Base Year<b>"

