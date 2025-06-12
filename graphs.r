library(RColorBrewer)
library(tidyverse)
library(viridis)
library(ggplot2)
library(patchwork)
library(gt)
library(gtable)


# Colors
colors1 <- brewer.pal(8, "Set2")
border_colors1 <- colorspace::darken(colors1, 0.2)
colors2 <- brewer.pal(11, "Set3")[-9]
border_colors2 <- colorspace::darken(colors2, 0.2)

# Read data
all_catch <- read.csv("~/R/Quarto/Pelagic_Presentation/data/CatchByFleet_ISC25.csv")

all_vessels <- read.csv("~/R/Quarto/Pelagic_Presentation/data/VesselsByFleet_ISC25.csv")

# Clean up 
removed_fleets <- c("Tropical PL", "Gillnet", "Harpoon")
removed_species <- c("ALV", "BIL", "BTH", "PTH", "SKH", "TUN")

fleet_names <- as.character(unique(all_catch$Fishery))

# Clean up numeric data by removing commas
all_vessels <- all_vessels %>%
  mutate(across(Purse.Seine:Other, ~ as.numeric(gsub(",", "", .))))

all_catch <- all_catch %>%
  mutate(across(ALB:TOTAL, ~ suppressWarnings(as.numeric(gsub(",", "", as.character(.)))))) %>% 
  mutate(across(ALB:TOTAL, ~replace_na(., 0)))

# Reshape data
all_vessels <- pivot_longer(all_vessels, Purse.Seine:Other, names_to = "fleet", values_to = "vessels")
all_vessels <- all_vessels %>% mutate(fleet = gsub("\\."," ",fleet))

clean_all_catch <- all_catch %>% filter(!(Fishery %in% removed_fleets)) %>% select(-removed_species) %>% filter(FISHERY.YEAR > 2019)
clean_all_vessels <- all_vessels %>% filter(!(fleet %in% removed_fleets)) %>% filter(Year > 2019)

individual_catch <- pivot_longer(all_catch, ALB:TOTAL, names_to ="species", values_to="catch") %>% select('FISHERY.YEAR', 'Fishery', 'species', 'catch') %>% mutate(catch = as.numeric(gsub(",","",catch)))
clean_individual_catch <- pivot_longer(clean_all_catch, ALB:TOTAL, names_to ="species", values_to="catch") %>% select('FISHERY.YEAR', 'Fishery', 'species', 'catch') %>% mutate(catch = as.numeric(gsub(",","",catch)))
## Time series matrix of number of vessels by fleet

# Initialize an empty list to collect plots
plot_list <- list()

for (i in 1:(length(fleet_names) - 2)) {
  
  fleet_name <- unique(all_vessels$fleet)[i]

  vessel_vals <- all_vessels %>%
    filter(fleet == fleet_name) %>%
    pull(vessels) %>%
    .[is.finite(.)]

  p <- ggplot(all_vessels %>% filter(fleet == fleet_name), aes(x = Year, y = vessels)) +
    geom_line(size = 0.5) +
    theme_minimal() +
    labs(title = fleet_names[i], y = "Number of Vessels") +
    theme(
      axis.text = element_text(size = 7),
      axis.title = element_text(size = 8),
      plot.title = element_text(size = 10, face="bold")
    ) +
    scale_y_continuous(breaks = pretty(range(0, vessel_vals), n = 5))

  plot_list[[i]] <- p
}

vessels_by_fleet <- wrap_plots(plotlist = plot_list) 

## Time series matrix of total catch by fleet

plot_list <- list()

for (i in 1:(length(unique(all_catch$Fishery)))) {
  
  fleet_name <- unique(all_catch$Fishery)[i]

  total_vals <- all_catch %>%
    filter(Fishery == fleet_name) %>%
    pull(TOTAL) %>%
    .[is.finite(.)]

  p <- ggplot(all_catch %>% filter(Fishery == fleet_name), aes(x = FISHERY.YEAR, y = TOTAL)) +
    geom_line(size = 0.5) +
    theme_minimal() +
    labs(title = fleet_name, y = "Total Catch (metric tons)", x = "Year") +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 15, face="bold")
    ) +
    scale_y_continuous(breaks = pretty(range(0, total_vals), n = 5))

  plot_list[[i]] <- p
}

totals_by_fleet <- wrap_plots(plotlist = plot_list)

## Time series matrix of total catch by species

catch_by_species <- individual_catch %>% group_by(species, FISHERY.YEAR) %>% summarise(sum = sum(catch, na.rm=TRUE), .groups="drop")

plot_list <- list()

for (i in 1:(length(unique(catch_by_species$species)))) {
  
  species_name <- unique(catch_by_species$species)[i]

  total_vals <- catch_by_species %>%
    filter(species == species_name) %>%
    pull(sum) %>%
    .[is.finite(.)]

  p <- ggplot(catch_by_species %>% filter(species == species_name), aes(x = FISHERY.YEAR, y = sum)) +
    geom_line(size = 0.5) +
    theme_minimal() +
    labs(title = species_name, y = "Total Catch", x = "Year") +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 15, face = "bold")
    ) +
    scale_y_continuous(breaks = pretty(range(0, total_vals), n = 5))

  plot_list[[i]] <- p
}

totals_by_species <- wrap_plots(plotlist = plot_list, ncol=4, nrow=5)

## Proportion of catch by gear for species (all years)

# Compute total catches per Fishery
totals <- individual_catch %>%
  filter(!(Fishery %in% removed_fleets)) %>% 
  filter(species == "TOTAL") %>%
  group_by(Fishery) %>%
  summarise(total_catch = sum(catch, na.rm = TRUE), .groups = "drop")

# Compute relative frequencies for catch of each species 
total_rel_freqs <- individual_catch %>%
  filter(!(Fishery %in% removed_fleets)) %>% 
  filter(species != "TOTAL") %>%
  filter(!(species %in% removed_species)) %>% 
  group_by(Fishery, species) %>%
  summarise(species_catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
  left_join(totals, by = "Fishery") %>%
  mutate(rel_freq = species_catch / total_catch)

# Create plot
frequency_by_fishery <- ggplot(total_rel_freqs, aes(x = species, y = rel_freq, fill = species, color = species)) +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = border_colors2) +
  geom_bar(stat = "identity", size=0.5) +
  facet_wrap(~ Fishery) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Species",  
    y = "Relative Frequency",
    fill = "Species"
  ) +
  guides(color = "none") +
  theme_minimal() +
  theme(
    plot.margin = margin(5, 30, 5, 30),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    panel.spacing = unit(15, "points"),
    strip.background.x = element_rect(fill = alpha("#1D63ED", 0.25), linetype = "solid", color = alpha("#1D63ED", 0.5), linewidth = 0.5),
    strip.text.x=element_text(face="bold", size=15)
  )
## Proportion of catch by gear for species (2015-2024)

# Compute total catches per Fishery
recent_totals <- clean_individual_catch %>%
  filter(species == "TOTAL") %>%
  group_by(Fishery) %>%
  summarise(recent_total_catch = sum(catch, na.rm = TRUE), .groups = "drop")

# Compute relative frequencies for catch of each species 
recent_rel_freqs <- clean_individual_catch %>%
  filter(species != "TOTAL") %>%
  group_by(Fishery, species) %>%
  summarise(recent_species_catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
  left_join(recent_totals, by = "Fishery") %>%
  mutate(recent_rel_freq = recent_species_catch / recent_total_catch)

# Create plot
recent_frequency_by_fishery <- ggplot(recent_rel_freqs, aes(x = species, y = recent_rel_freq, fill = species, color = species)) +
  scale_fill_manual(values = colors2) +
  scale_color_manual(values = border_colors2) +
  geom_bar(stat = "identity", size=0.5) +
  facet_wrap(~ Fishery) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Species",
    y = "Relative Frequency",
    fill = "Species"
  ) +
  guides(color = "none") +
  theme_minimal() +
  theme(
    plot.margin = margin(5, 30, 5, 30),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 13),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 13),
    panel.spacing = unit(15, "points"),
    strip.background.x = element_rect(fill = alpha("#1D63ED", 0.25), linetype = "solid", color = alpha("#1D63ED", 0.5), linewidth = 0.5),
    strip.text.x=element_text(face="bold", size=15)
  )

## Proportion of catch for species by gear (all years)

# Compute total catches per species
species_totals <- individual_catch %>%
  filter(!(Fishery %in% removed_fleets)) %>% 
  filter(!(species %in% removed_species)) %>% 
  group_by(species) %>%
  summarise(total_catch = sum(catch, na.rm = TRUE), .groups = "drop")

# Compute relative frequencies for catch of each gear 
species_freqs <- individual_catch %>%
  filter(!(Fishery %in% removed_fleets)) %>% 
  filter(!(species %in% removed_species)) %>% 
  group_by(species, Fishery) %>%
  summarise(species_catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
  left_join(species_totals, by = "species") %>%
  mutate(species_freq = species_catch / total_catch)

# Create plot
frequency_by_species <- ggplot(species_freqs, aes(x = Fishery, y = species_freq, fill = Fishery, color = Fishery)) +
  scale_fill_manual(values = colors1) +
  scale_color_manual(values = border_colors1) +
  geom_bar(stat = "identity", size=0.5) +
  facet_wrap(~ species) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Relative Frequency",
    fill = "Fleet"
  ) +
  guides(color = "none") +
  theme_minimal() +
  theme(
    plot.margin = margin(5, 20, 5, 20),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    panel.spacing = unit(15, "points"),
    strip.background.x = element_rect(fill = alpha("#1D63ED", 0.25), linetype = "solid", color = alpha("#1D63ED", 0.5), linewidth = 0.5),
    strip.text.x=element_text(face="bold", size=13)
  )
## Proportion of catch for species by gear (2015-2024)

# Compute total catches per species
recent_species_totals <- clean_individual_catch %>%
  group_by(species) %>%
  summarise(recent_total_catch = sum(catch, na.rm = TRUE), .groups = "drop")

# Compute relative frequencies for catch of each gear 
recent_species_freqs <- clean_individual_catch %>%
  group_by(species, Fishery) %>%
  summarise(recent_species_catch = sum(catch, na.rm = TRUE), .groups = "drop") %>%
  left_join(recent_species_totals, by = "species") %>%
  mutate(recent_species_freq = recent_species_catch / recent_total_catch)

# Create plot
recent_frequency_by_species <- ggplot(recent_species_freqs, aes(x = Fishery, y = recent_species_freq, fill = Fishery, color = Fishery)) +
  scale_fill_manual(values = colors1) +
  scale_color_manual(values = border_colors1) +
  geom_bar(stat = "identity", size=0.5) +
  facet_wrap(~ species) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Relative Frequency",
    fill = "Fleet"
  ) +
  guides(color = "none") +
  theme_minimal() +
  theme(
    plot.margin = margin(5, 20, 5, 20),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    panel.spacing = unit(15, "points"),
    strip.background.x = element_rect(fill = alpha("#1D63ED", 0.25), linetype = "solid", color = alpha("#1D63ED", 0.5), linewidth = 0.5),
    strip.text.x=element_text(face="bold", size=13)
  )

# Area plot for Longline 
longline_species <- c("BET", "SWO", "YFT", "ALB", "MLS", "BUM")

longline <- individual_catch %>% filter(Fishery == "Longline") %>% 
  filter(species %in% longline_species) %>% 
  select(-2) %>% 
  filter(FISHERY.YEAR > 1986)

yearly_totals <- all_catch %>% 
  filter(Fishery == "Longline") %>%
  filter(FISHERY.YEAR > 1986) %>% 
  group_by(FISHERY.YEAR) %>% 
  select(TOTAL)

longline <- longline %>% left_join(yearly_totals, by="FISHERY.YEAR") %>% 
  mutate(prop = catch/TOTAL) %>% complete()

longline_prop <- ggplot(longline, aes(x=FISHERY.YEAR, y=prop, fill=species)) +
  geom_area(color="white", alpha=0.75, linewidth=0.6) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Proportion of Total Catch",
    fill = "Species"
  ) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.box.spacing = unit(5, "points"),
    axis.text.x = element_text(vjust = 0, size=10),
    axis.text.y = element_text(hjust = 0, size=10),
    axis.title.x = element_text(face="bold", size=12, margin=margin(t=5)),
    axis.title.y = element_text(face="bold", size=12, margin=margin(r=10)),
    legend.margin = margin(0, 0, 0, -15)
  )

longline_table <- all_catch %>% filter(Fishery == "Longline") %>% 
  select(FISHERY.YEAR, !!longline_species, TOTAL) %>% 
  arrange(desc(FISHERY.YEAR))

colnames(longline_table) <- c("Year", "ALB", "BET", "BUM", "MLS", "SWO", "YFT", "Total")

# Area plot for Purse Seine
purse_seine_species <- c("YFT", "SKJ", "PBF", "BET", "ALB")

purse_seine <- individual_catch %>% filter(Fishery == "Purse Seine") %>% 
  filter(species != "TOTAL") %>% 
  select(-2)

yearly_totals <- all_catch %>% 
  filter(Fishery == "Purse Seine") %>%
  group_by(FISHERY.YEAR) %>% 
  select(TOTAL)

purse_seine <- purse_seine %>% left_join(yearly_totals, by="FISHERY.YEAR") %>% 
  mutate(prop = catch/TOTAL) %>% 
  filter(species %in% purse_seine_species)

turbo <- turbo(7)[2:6]

purse_seine_prop <- ggplot(purse_seine, aes(x=FISHERY.YEAR, y=prop, fill=species)) +
  geom_area(color="white", alpha=0.75, na.rm=TRUE, linewidth=0.6) +
  scale_fill_manual(values=turbo) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Proportion of Total Catch",
    fill = "Species"
  ) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.box.spacing = unit(5, "points"),
    axis.text.x = element_text(vjust = 0, size=10),
    axis.text.y = element_text(hjust = 0, size=10),
    axis.title.x = element_text(face="bold", size=12, margin=margin(t=5)),
    axis.title.y = element_text(face="bold", size=12, margin=margin(r=10)),
    legend.margin = margin(0, 0, 0, -15)
  )

purse_seine_table <- all_catch %>% filter(Fishery == "Purse Seine") %>% 
  select(FISHERY.YEAR, !!purse_seine_species, TOTAL) %>% 
  arrange(desc(FISHERY.YEAR))

colnames(purse_seine_table) <- c("Year", "ALB", "BET", "PBF", "SKJ", "YFT", "Total")

# Area plot for Tropical Troll
tropical_troll_species <- c("ALB", "BET", "BUM", "SKJ", "YFT")

tropical_troll <- individual_catch %>% filter(Fishery == "Tropical Troll") %>% 
  filter(species != "TOTAL") %>% 
  select(-2)

yearly_totals <- all_catch %>% 
  filter(Fishery == "Tropical Troll") %>%
  group_by(FISHERY.YEAR) %>% 
  select(TOTAL)

tropical_troll <- tropical_troll %>% left_join(yearly_totals, by="FISHERY.YEAR") %>% 
  mutate(prop = catch/TOTAL) %>% 
  filter(species %in% tropical_troll_species)

tropical_troll_prop <- ggplot(tropical_troll, aes(x=FISHERY.YEAR, y=prop, fill=species)) +
  geom_area(color="white", alpha=0.75, na.rm=TRUE, linewidth=0.6) +
  scale_fill_viridis(option = "plasma", discrete = TRUE) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Proportion of Total Catch",
    fill = "Species"
  ) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.box.spacing = unit(5, "points"),
    axis.text.x = element_text(vjust = 0, size=10),
    axis.text.y = element_text(hjust = 0, size=10),
    axis.title.x = element_text(face="bold", size=12, margin=margin(t=5)),
    axis.title.y = element_text(face="bold", size=12, margin=margin(r=10)),
    legend.margin = margin(0, 0, 0, -15)
  )

tropical_troll_table <- all_catch %>% filter(Fishery == "Tropical Troll") %>% 
  select(FISHERY.YEAR, !!tropical_troll_species, TOTAL) %>% 
  arrange(desc(FISHERY.YEAR))

colnames(tropical_troll_table) <- c("Year", "ALB", "BET", "BUM", "SKJ", "YFT", "Total")

# Area plot for Sport
sport_species <- c("ALB", "MLS", "PBF", "YFT")

sport <- individual_catch %>% filter(Fishery == "Sport") %>% 
  filter(species != "TOTAL") %>% 
  select(-2)

yearly_totals <- all_catch %>% 
  filter(Fishery == "Sport") %>%
  group_by(FISHERY.YEAR) %>% 
  select(TOTAL)

sport <- sport %>% left_join(yearly_totals, by="FISHERY.YEAR") %>% 
  mutate(prop = catch/TOTAL) %>% 
  filter(species %in% sport_species)

sport_table <- all_catch %>% filter(Fishery == "Sport") %>% 
  select(FISHERY.YEAR, !!sport_species, TOTAL) %>% 
  arrange(desc(FISHERY.YEAR))

colnames(sport_table) <- c("Year", "ALB", "MLS", "PBF", "YFT", "Total")

mako <- mako(5)[-1]

sport_prop <- ggplot(sport, aes(x=FISHERY.YEAR, y=prop, fill=species)) +
  geom_area(color="white", alpha=0.75, linewidth=0.6) +
  scale_fill_manual(values = mako) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Proportion of Total Catch",
    fill = "Species"
  ) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.box.spacing = unit(5, "points"),
    axis.text.x = element_text(vjust = 0, size=10),
    axis.text.y = element_text(hjust = 0, size=10),
    axis.title.x = element_text(face="bold", size=12, margin=margin(t=5)),
    axis.title.y = element_text(face="bold", size=12, margin=margin(r=10)),
    legend.margin = margin(0, 0, 0, -15)
  )

# Area plot for yellowfin tuna
yellowfin_vessels <- c("Longline", "Purse Seine", "Sport", "Tropical PL", "Tropical Troll")

yellowfin <- individual_catch %>% filter(species=="YFT") %>% 
  select(-3)
  
yearly_totals <- individual_catch %>% filter(species == "YFT") %>% 
  group_by(FISHERY.YEAR) %>% 
  summarise(yellowfin_total = sum(catch, na.rm=TRUE), .groups="drop")

yellowfin <- yellowfin %>% left_join(yearly_totals, by="FISHERY.YEAR") %>% 
  mutate(prop = catch/yellowfin_total) %>% 
  filter(Fishery %in% yellowfin_vessels) 

yellowfin_prop <- ggplot(yellowfin, aes(x=FISHERY.YEAR, y=prop, fill=Fishery)) +
  geom_area(color="white", alpha=0.75, linewidth=0.6) +
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Proportion of Total Catch",
    fill = "Fishery"
  ) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.box.spacing = unit(5, "points"),
    axis.text.x = element_text(vjust = 0, size=10),
    axis.text.y = element_text(hjust = 0, size=10),
    axis.title.x = element_text(face="bold", size=12, margin=margin(t=5)),
    axis.title.y = element_text(face="bold", size=12, margin=margin(r=10)),
    legend.margin = margin(0, 0, 0, -15)
  )

# Area plot for pacific bluefin tuna
bluefin_vessels <- c("Gillnet", "Other", "Purse Seine", "Sport", "Surface hook and line")

bluefin <- individual_catch %>% filter(species=="PBF") %>% 
  select(-3)

yearly_totals <- individual_catch %>% filter(species == "PBF") %>% 
  group_by(FISHERY.YEAR) %>% 
  summarise(bluefin_total = sum(catch, na.rm=TRUE), .groups="drop")

bluefin <- bluefin %>% left_join(yearly_totals, by="FISHERY.YEAR") %>% 
  mutate(prop = catch/bluefin_total) %>% 
  filter(Fishery %in% bluefin_vessels) 

bluefin_prop <- ggplot(bluefin, aes(x=FISHERY.YEAR, y=prop, fill=Fishery)) +
  geom_area(color="white", alpha=0.75, linewidth=0.6) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Proportion of Total Catch",
    fill = "Fishery"
  ) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.box.spacing = unit(5, "points"),
    axis.text.x = element_text(vjust = 0, size=10),
    axis.text.y = element_text(hjust = 0, size=10),
    axis.title.x = element_text(face="bold", size=12, margin=margin(t=5)),
    axis.title.y = element_text(face="bold", size=12, margin=margin(r=10)),
    legend.margin = margin(0, 0, 0, -15)
  )

# Area plot for all species
total_vessels <- c("ALB Troll and PL", "Longline", "Purse Seine", "Sport", "Tropical Troll")

all_species <- individual_catch %>% filter(species=="TOTAL") %>% 
  select(-3)

yearly_totals <- individual_catch %>% filter(species == "TOTAL") %>% 
  group_by(FISHERY.YEAR) %>% 
  summarise(all_species_total = sum(catch, na.rm=TRUE), .groups="drop")

all_species <- all_species %>% left_join(yearly_totals, by="FISHERY.YEAR") %>% 
  mutate(prop = catch/all_species_total) %>% 
  filter(Fishery %in% total_vessels) 

all_species_prop <- ggplot(all_species, aes(x=FISHERY.YEAR, y=prop, fill=Fishery)) +
  geom_area(color="white", alpha=0.75, linewidth=0.6) +
  scale_fill_viridis(discrete = TRUE, option = "mako") +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Proportion of Total Catch",
    fill = "Fishery"
  ) +
  theme(
    plot.margin = margin(10, 10, 10, 10),
    legend.box.spacing = unit(5, "points"),
    axis.text.x = element_text(vjust = 0, size=10),
    axis.text.y = element_text(hjust = 0, size=10),
    axis.title.x = element_text(face="bold", size=12, margin=margin(t=5)),
    axis.title.y = element_text(face="bold", size=12, margin=margin(r=10)),
    legend.margin = margin(0, 0, 0, -15)
  )

#all_species_table <- all_catch %>% filter(Fishery %in% total_vessels) %>%
  #filter(Fishery != "Tropical PL") %>% 
  #select(FISHERY.YEAR, TOTAL)

#colnames(all_species_table) <- c("Year", "ALB", "Longline", "Purse Seine", "Sport", "Tropical Troll")
