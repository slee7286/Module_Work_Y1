library(tidyverse)
library(janitor)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

setwd("C:/Users/slee7/Downloads")

# China Investment Plot

inv <- read.csv("WorldEnergyInvestment2025_DataFile.csv", stringsAsFactors = F)
inv <- inv[c(1:29), c(1:12)]
inv[1,1] <- "Year"
inv[3,1] <- "Renewable Energy"

inv <- na.omit(inv)

rownames(inv) <- inv[[1]]

inv <- inv[, -1, drop = FALSE]

inv_wide <- as.data.frame(t(inv), stringsAsFactors = FALSE)

inv_wide <- inv_wide %>%
  pivot_longer(
    cols = c(`Renewable Energy`, Oil, Gas, Coal, `Clean Fuels`),
    names_to = "energy_type",
    values_to = "amount"
  ) %>%
  group_by(Year) %>%
  mutate(
    pct = amount / `Total Billion USD (2024, MER)`
  ) %>%
  ungroup()

inv_label <- inv_wide %>%
  group_by(Year) %>%
  filter(energy_type == "Renewable Energy") %>%
  mutate(
    # height of the green segment top
    seg_top = cumsum(amount),    # see note below
    # put label at, e.g., 75% of that green height
    y_label = seg_top * 0.5     # lower/higher by changing 0.75
  )

inv_wide %>% 
  ggplot(aes(x = Year, y = amount, fill = energy_type)) +
  geom_bar(stat = "identity") +
  labs(
    x    = "Year",
    y    = "Investment amount (2024 USD)",
    fill = "Energy Type",
    title = "China Energy Investment by Type (2015-25)"
      ) +
  geom_text(
    data = inv_label,
    aes(
      x     = Year,
      y     = y_label,
      label = scales::percent(pct, accuracy = 1)
    ),
    color = "black",
    size  = 3.5
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw()

# US Investment Plot

inv <- read.csv("WorldEnergyInvestment2025_DataFile1.csv", stringsAsFactors = F)
inv <- inv[c(1:29), c(1:12)]
inv[1,1] <- "Year"
inv[3,1] <- "Renewable Energy"

inv <- na.omit(inv)

rownames(inv) <- inv[[1]]

inv <- inv[, -1, drop = FALSE]

inv_wide <- as.data.frame(t(inv), stringsAsFactors = FALSE)

inv_wide <- inv_wide %>%
  pivot_longer(
    cols = c(`Renewable Energy`, Oil, Gas, Coal, `Clean Fuels`),
    names_to = "energy_type",
    values_to = "amount"
  ) %>%
  group_by(Year) %>%
  mutate(
    pct = amount / `Total Billion USD (2024, MER)`
  ) %>%
  ungroup()

inv_label <- inv_wide %>%
  group_by(Year) %>%
  filter(energy_type == "Renewable Energy") %>%
  mutate(
    # height of the green segment top
    seg_top = cumsum(amount),    # see note below
    # put label at, e.g., 75% of that green height
    y_label = seg_top * 0.5     # lower/higher by changing 0.75
  )

inv_wide %>% 
  ggplot(aes(x = Year, y = amount, fill = energy_type)) +
  geom_bar(stat = "identity") +
  labs(
    x    = "Year",
    y    = "Investment amount (2024 USD)",
    fill = "Energy Type",
    title = "US Energy Investment by Type (2015-25)"
  ) +
  geom_text(
    data = inv_label,
    aes(
      x     = Year,
      y     = y_label,
      label = scales::percent(pct, accuracy = 1)
    ),
    color = "black",
    size  = 3.5
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw()

# US Renewable Energy Consumption by Source
ren <- read.csv("Renewable Energy Consumption.csv", stringsAsFactors = F)

ren <- ren %>%
  filter(Value != "Not Available") %>%
  mutate(Value = as.numeric(Value))


ren <- ren[ren$YYYYMM %% 100 != 13, ]

ren$YYYYMM <- as.Date(
  paste0(ren$YYYYMM, "01"),   # make YYYYMM01
  format = "%Y%m%d"
)

ren$Description <- trimws(ren$Description)

ren$Description[ren$Description == "Solar Energy Consumed by the Electric Power Sector"] <- "Solar"

ren$Description[ren$Description == "Wood Energy Consumed by the Electric Power Sector"] <- "Wood"

ren$Description[ren$Description == "Wind Energy Consumed by the Electric Power Sector"] <- "Wind"

ren$Description[ren$Description == "Conventional Hydroelectric Power Consumed by the Electric Power Sector"] <- "Hydroelectric Power"

ren$Description[ren$Description == "Geothermal Energy Consumed by the Electric Power Sector"] <- "Geothermal"

ren <- ren %>%
  filter(ren$Description != "Total Renewable Energy Consumed by the Electric Power Sector",
         ren$Description != "Waste Energy Consumed by the Electric Power Sector",
         ren$Description != "Wood")

ren <- ren %>%
  mutate(
    Value = if_else(Description == "Wood", Value * 10, Value),
    Unit = if_else(Description == "Wood", "Quadrillion Btu", Unit)
  )


labels_to_merge <- c("Biodiesel Consumed by the Transportation Sector",
                     "Biomass Energy Consumed by the Electric Power Sector",
                     "Biomass Energy Consumed by the Transportation Sector",
                     "Fuel Ethanol, Excluding Denaturant, Consumed by the Transportation Sector",
                     "Other Biofuels Consumed by the Transportation Sector",
                     "Renewable Diesel Fuel Consumed by the Transportation Sector")

ren_agg <- ren %>%
  group_by(YYYYMM, Description) %>%
  summarise(Value = sum(Value, na.rm = TRUE),.groups = "drop")

sum_rows <- ren_agg %>%
  filter(Description %in% labels_to_merge) %>%
  group_by(YYYYMM) %>%
  summarise(
    Description = "Biofuels",
    Value       = sum(Value, na.rm = TRUE),.groups     = "drop"
  )

df_new <- ren %>%
  subset(!(Description %in% labels_to_merge)) %>%
  bind_rows(sum_rows)


df_new <- df_new %>%
  mutate(Year = year(YYYYMM)) %>%
  group_by(Year, Description) %>%
  summarise(Value = sum(Value))

df_new %>% 
  ggplot(aes(x = Year, y = Value, color = Description)) +
  geom_line(aes(group = Description), linewidth = 1.5) +
  labs(
    x    = "Year",
    y    = "Energy Consumption (Trillion Btu)",
    title = "US Renewable Energy Consumption by Source",
    legend = "Major Sources"
  ) +
  theme_bw()

# US Primary Energy Consumption by Source

ren <- read.csv("Renewable Energy Consumption.csv", stringsAsFactors = F)

ren <- ren %>%
  filter(Value != "Not Available") %>%
  mutate(Value = as.numeric(Value))


ren <- ren[ren$YYYYMM %% 100 != 13, ]

ren$YYYYMM <- as.Date(
  paste0(ren$YYYYMM, "01"),   # make YYYYMM01
  format = "%Y%m%d"
)

ren$Description <- trimws(ren$Description)

ren$Description[ren$Description == "Solar Energy Consumed by the Electric Power Sector"] <- "Solar"

ren$Description[ren$Description == "Wood Energy Consumed by the Electric Power Sector"] <- "Wood"

ren$Description[ren$Description == "Wind Energy Consumed by the Electric Power Sector"] <- "Wind"

ren$Description[ren$Description == "Conventional Hydroelectric Power Consumed by the Electric Power Sector"] <- "Hydroelectric Power"

ren$Description[ren$Description == "Geothermal Energy Consumed by the Electric Power Sector"] <- "Geothermal"

ren <- ren %>%
  filter(ren$Description != "Total Renewable Energy Consumed by the Electric Power Sector",
         ren$Description != "Waste Energy Consumed by the Electric Power Sector",
         ren$Description != "Wood")

ren <- ren %>%
  mutate(
    Value = if_else(Description == "Wood", Value * 10, Value),
    Unit = if_else(Description == "Wood", "Quadrillion Btu", Unit)
  )


labels_to_merge <- c("Biodiesel Consumed by the Transportation Sector",
                     "Biomass Energy Consumed by the Electric Power Sector",
                     "Biomass Energy Consumed by the Transportation Sector",
                     "Fuel Ethanol, Excluding Denaturant, Consumed by the Transportation Sector",
                     "Other Biofuels Consumed by the Transportation Sector",
                     "Renewable Diesel Fuel Consumed by the Transportation Sector")

ren_agg <- ren %>%
  group_by(YYYYMM, Description) %>%
  summarise(Value = sum(Value, na.rm = TRUE),.groups = "drop")

sum_rows <- ren_agg %>%
  filter(Description %in% labels_to_merge) %>%
  group_by(YYYYMM) %>%
  summarise(
    Description = "Biofuels",
    Value       = sum(Value, na.rm = TRUE),.groups     = "drop"
  )

df_new <- ren %>%
  subset(!(Description %in% labels_to_merge)) %>%
  bind_rows(sum_rows)


df_new <- df_new %>%
  mutate(Year = year(YYYYMM)) %>%
  group_by(Year, Description) %>%
  summarise(Value = sum(Value))

df_new %>% 
  ggplot(aes(x = Year, y = Value, color = Description)) +
  geom_line(aes(group = Description), linewidth = 1.5) +
  labs(
    x    = "Year",
    y    = "Energy Consumption (Trillion Btu)",
    title = "US Renewable Energy Consumption by Source",
    legend = "Major Sources"
  ) +
  theme_bw()