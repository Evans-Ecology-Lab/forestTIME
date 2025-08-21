library(rFIA)
library(tidyverse)
rfia_byplot <- rFIA::fiaRI |>
  biomass(
    totals = TRUE,
    method = "TI",
    treeType = "live",
    landType = 'forest',
    component = "AG",
    areaDomain = COND_STATUS_CD == 1 & INTENSITY == 1,
    byPlot = TRUE
  )

db <- fia_load(
  "RI",
  dir = system.file("exdata", package = "forestTIME")
)
data <- fia_tidy(db) #single tibble
data_midpt <- data |>
  fia_annualize(use_mortyr = FALSE) |>
  fia_estimate()

ft_plots <- data_midpt |>
  filter(COND_STATUS_CD == 1 & INTENSITY == 1, STATUSCD == 1) |>
  pull(plot_ID) |>
  unique()

# rfia's pltID =
# UNITCD_STATECD_COUNTYCD_PLOT

rfia_byplot <- rfia_byplot |>
  separate_wider_delim(
    pltID,
    delim = "_",
    names = c("UNITCD", "STATECD", "COUNTYCD", "PLOT")
  ) |>
  fia_add_composite_ids()

rfia_plots <- rfia_byplot$plot_ID |> unique()

setdiff(ft_plots, rfia_byplot)


data_midpt |> filter(plot_ID == "44_1_1_228") |> View()


fiaRI$PLOT |> fia_add_composite_ids() |> filter(plot_ID == "44_1_1_228")
