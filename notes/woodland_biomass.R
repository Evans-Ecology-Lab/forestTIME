library(nanoparquet)
library(readr)
library(dplyr)
az <- read_parquet("AZ/AZ_midpt.parquet")
#take a sample of 100 woodland trees
az_tree_sample <- az |>
  filter(JENKINS_SPGRPCD == 10) |>
  pull(tree_ID) |>
  unique() |>
  sample(100)
az_sample <- az |> filter(tree_ID %in% az_tree_sample)

az_sample

# Biomass for woodland species < 5.0 inches in diameter and saplings
# (non-woodland species between 1.0 to 4.9 inches in diameter) are estimated
# directly from the biomass equation in Jenkins et al. (2003 and 2004) using the
# observed diameter and a sapling adjustment factor. Biomass for woodland
# species ≥ 5.0 inches diameter is based on volume estimates for the entire
# aboveground portion of the sample tree with no component breakdown.
# (Woodall et al. 2011)

# if DIA ≥ 5, use Woodall et al. 2011 with no component breakdown
REF_SPECIES <- read_csv("data-raw/REF_SPECIES.csv")

az_sample |>
  left_join(REF_SPECIES) |>
  pull(JENKINS_SAPLING_ADJUSTMENT) |>
  unique()
mutate(
  DRYBIO_AG = case_when(
    DIA < 5 ~ exp(-0.7152 + 1.7029 * log(DIA)) * JENKINS_SAPLING_ADJUSTMENT,
    # DIA >= 5 ~
  )
) |>
  select(DRYBIO_AG, everything())

exp(-0.7152 + 1.7029 * log(4.5))
