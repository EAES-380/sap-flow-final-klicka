Project Visuals
================
2025-11-19

``` r
library(tidyverse)
library(broom)
library(readr)
```

## 1. Data

``` r
uic_elm_0b <-read.csv("data/250801_251031_sx61na0b_uic_american_elm_sap_flow.csv")%>%
  mutate(tree_species = "American elm", site = "UIC")

uic_elm_0o <-read.csv("data/250801_251031_sx61na0o_uic_american_elm_sap_flow.csv")%>%
  mutate(tree_species = "American elm", site = "UIC") 

uic_honeylocust <-read.csv("data/250801_251031_sx61na0r_uic_honey_locust_sap_flow.csv")%>%
  mutate(tree_species = "Honey locust", site = "UIC")

nu_maple_0f <-read.csv("data/250801_251031_sx61na0f_nu_maple_sap_flow.csv")%>%
  mutate(tree_species = "Maple", site = "Northwestern") 

nu_maple_0l <-read.csv("data/250801_251031_sx61na0l_nu_maple_sap_flow.csv")%>%
  mutate(tree_species = "Maple", site = "Northwestern") 

nu_oak_0g <-read.csv("data/250801_251031_sx61na0g_nu_oak_sap_flow.csv")%>%
  mutate(tree_species = "Oak", site = "Northwestern") 

nu_oak_07 <-read.csv("data/250801_251031_sx61na07_nu_oak_sap_flow.csv")%>%
  mutate(tree_species = "Oak", site = "Northwestern") 
```

Joining all of our data sets

``` r
sap_all_raw <- bind_rows(
  uic_elm_0b,
  uic_elm_0o,
  uic_honeylocust,
  nu_maple_0f,
  nu_maple_0l,
  nu_oak_0g,
  nu_oak_07
)
```

If we want to discuss sap flux, let’s mutate our variables to produce a
sap_flux variable. Also, I’m cleaning up some other variable names here.

``` r
sap_all <- sap_all_raw %>%
  rename(
    datetime = Timestamp,
    inner = uncorrected_inner..cm.hr.,
    outer = uncorrected_outer..cm.hr.) %>%
  mutate(
    datetime = ymd_hms(datetime),

    # calculate sap flux (cm/hr)
    sap_flux = rowMeans(cbind(inner, outer), na.rm = TRUE),

    # time variables for analysis
    date = as.Date(datetime),
    hour = hour(datetime),
    time = format(datetime, "%H:%M:%S"))
```

Here, I am selecting the variables that I want to show in our final
wrangled data set “sap_all”.

``` r
sap_all <- sap_all%>%
  select(
    date,
    time,
    hour,
    site,
    tree_species,
    inner,
    outer,
    sap_flux
  )
```

## 2. Visualization

Answering our first research question: “Average amount of sap flow by an
hour interval”

``` r
sap_hourly <- sap_all %>%
  group_by(hour) %>%
  summarize(mean_sap_flux = mean(sap_flux, na.rm = TRUE), .groups = "drop")

ggplot(sap_hourly, aes(x = hour, y = mean_sap_flux)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average sap flux by hour of day",
    x = "Hour of day",
    y = "Mean sap flux (cm/hr)"
  ) +
  theme_classic()
```

![](Project_Starter_files/figure-gfm/avg-hrly-sap-flow-Q1-1.png)<!-- -->

Answering our second research question: “Which species has the
highest/lowest sap flow rates?”

``` r
sap_hourly_species <- sap_all %>%
  group_by(tree_species, hour) %>%
  summarize(mean_sap_flux = mean(sap_flux, na.rm = TRUE), .groups = "drop")

ggplot(sap_hourly_species, aes(x = hour, y = mean_sap_flux, color = tree_species)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average sap flux by hour of day, by species",
    x = "Hour of day",
    y = "Mean sap flux (cm/hr)",
    color = "Tree species"
  ) +
  theme_classic()
```

![](Project_Starter_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Answering our third research question: “What effects sap flow rates
more, location or species?”

Bar chart showing the average sap flow by site, UIC v Northwestern.

``` r
site_effect <- sap_all %>%
  group_by(site) %>%
  summarize(mean_flow = mean(sap_flux, na.rm = TRUE))
ggplot(site_effect, aes(x = site, y = mean_flow, fill = site)) +
  geom_col() +
  labs(
    title = "Mean Sap Flow by Site",
    x = "Site",
    y = "Mean Sap Flux (cm/hr)"
  ) +
  theme_classic() +
  theme(legend.position = "none")
```

![](Project_Starter_files/figure-gfm/site-effect-sap-rate-1.png)<!-- -->

Bar chart showing the average sap flow by Tree Species.

``` r
species_effect <- sap_all %>%
  group_by(tree_species) %>%
  summarize(mean_flow = mean(sap_flux, na.rm = TRUE))
  
ggplot(species_effect, aes(x = tree_species, y = mean_flow, fill = tree_species)) +
  geom_col() +
  labs(
    title = "Mean Sap Flow by Tree Species",
    x = "Tree Species",
    y = "Mean Sap Flux (cm/hr)"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
```

![](Project_Starter_files/figure-gfm/species-effect-sap-rate-1.png)<!-- -->

Our final question: “Does time play a factor for sap flow accumulation”

``` r
daily_flow <- sap_all %>%
  group_by(tree_species, date) %>%
  summarize(daily_total = sum(sap_flux, na.rm = TRUE), .groups = "drop")
ggplot(daily_flow, aes(x = date, y = daily_total, color = tree_species)) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Daily Sap Flow by Tree Species",
    subtitle = "August 01 - October 31, 2025",
    x = "Date",
    y = "Daily Total Sap Flux (cm/hr)",
    color = "Tree Species"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
```

![](Project_Starter_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
