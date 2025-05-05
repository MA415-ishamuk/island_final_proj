library(tidyr)
library(dplyr)
library(stringr)

# data cleaning
# read in dataset - Health Indicators of Madagascar
health_ind <- read_csv("health_ind_mdg.csv", 
                       col_names = TRUE,
                       show_col_types = FALSE)

source("my_functions.R")

# check if read in table has any columns with only one value and remove those
# prt_val = T to show which columns had only one value and were dropped
mdg_ind <- health_ind |> drop_one_value_col(prt_val = T)

# remove columns that are not of interest for further data analysis
mdg_ind <- mdg_ind |> select(-`Indicator Code`)

# reorganize table so that years are rows rather than columns
mdg_long <- mdg_ind %>%
  pivot_longer(
    cols = `1960`:`2024`,   
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(Year = as.integer(Year))  # convert year column to integer

### create a separate table with just population information
mdg_pop <- mdg_long %>%
  filter(`Indicator Name` == "Population, total")

# remove the first column that has only one value and remove last row with no info
mdg_pop <- mdg_pop |> drop_one_value_col()
mdg_pop <- mdg_pop[-65,]

### create a separate table with just life expectancy information (male, female, total)
mdg_life_ex <- mdg_long %>%
  filter(str_starts(`Indicator Name`, "Life expectancy"))

# clean life expectancy table - remove redundant row names to leave just (female/male/total)
mdg_life_ex <- mdg_life_ex |> separate_wider_delim(cols = `Indicator Name`,
                                                   delim = ', ',
                                                   names = c("rem",
                                                             "Gender"),
                                                   too_many = "error",
                                                   too_few = "align_start")

mdg_life_ex <- mdg_life_ex |> separate_wider_delim(cols = `Gender`,
                                                   delim = ' ',
                                                   names = c("Gender",
                                                             "remove"),
                                                   too_many = "error",
                                                   too_few = "align_start")

mdg_life_ex <- mdg_life_ex |> select(-rem, -remove)

# pivot the table wider so that each classification (female/male/total) has its own column
mdg_life_ex <- pivot_wider(mdg_life_ex, names_from = Gender, values_from = Value)
mdg_life_ex <- mdg_life_ex[-65, ]

###  create a separate table with just infant mortality rate information
mdg_inf_mort <- mdg_long %>%
  filter(str_starts(`Indicator Name`, "Mortality rate, infant" ))


# clean infant mortality rate table - remove redundant row names to leave just (female/male/total)
mdg_inf_mort <- mdg_inf_mort |> separate_wider_delim(cols = `Indicator Name`,
                                                   delim = 'rate, ',
                                                   names = c("rem",
                                                             "Gender"),
                                                   too_many = "error",
                                                   too_few = "align_start")
mdg_inf_mort <- mdg_inf_mort %>%
  mutate(Gender = if_else(str_starts(Gender, fixed("infant (")), 
                          "infant, total (per 1,000 live births)", 
                          Gender))

mdg_inf_mort <- mdg_inf_mort |> separate_wider_delim(cols = `Gender`,
                                                   delim = 'infant, ',
                                                   names = c("remove",
                                                             "Gender"),
                                                   too_many = "error",
                                                   too_few = "align_start")


mdg_inf_mort <- mdg_inf_mort |> separate_wider_delim(cols = `Gender`,
                                                     delim = ' ',
                                                     names = c("Gender",
                                                               "remove2"),
                                                     too_many = "drop",
                                                     too_few = "align_start")

mdg_inf_mort <- mdg_inf_mort |> select(-rem, -remove, -remove2)

# pivot the table wider so that each classification (female/male/total) has its own column
mdg_inf_mort <- pivot_wider(mdg_inf_mort, names_from = Gender, values_from = Value)

# remove rows where all columns are NA
mdg_inf_mort <- mdg_inf_mort %>%
  filter(!is.na(total) & !is.na(female) & !is.na(male))

# read in dataset - Religions of Madagascar
mdg_religion <- read_csv("mdg_religion.csv", 
                         col_names = TRUE,
                         show_col_types = FALSE)

# keep only the column describing the religions within Madagascar
mdg_religion <- mdg_religion |> select(-`Eastern Africa[x]`, -`The World[x]`)

# filter through the religions to only keep the major religions (remove the comprising sects)
mdg_religion <- mdg_religion %>% filter(!str_starts(Religion, "--"))
mdg_religion <- mdg_religion %>% filter(!str_starts(`Madagascar[x]`, "--"))
mdg_religion <- mdg_religion[-7, ]

# make the percents in the Madagascar column numerical (to translate to the pie chart)
mdg_religion$`Madagascar[x]` <- as.numeric(gsub("%", "", mdg_religion$`Madagascar[x]`))