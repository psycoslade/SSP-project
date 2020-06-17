# Smart Service Project
# Descriptive Statistics of added variables

# set working directory

setwd(paste0("/Users/Daniel/Documents/Master/Studium/",
"1. Semester/Period 4/Smart Service Project/data"))

# load libraries

packages <- c("stargazer", "dplyr", "ggplot2", "cbsodataR",
"stringr", "geojsonio", "tigris", "data.table", "skimr")

sapply(packages, require, character.only = TRUE)

# check cbs data

df_cbs_toc <- cbs_get_toc() %>% filter(str_detect(Title, "Kerncijfers"))

View(df_cbs_toc)

#  load cbs data
data <- cbs_get_data("83765NED",
    select = c("Gemeentenaam_1", "SoortRegio_2",
    "AantalInwoners_5", "k_0Tot15Jaar_8", "k_15Tot25Jaar_9",
    "k_25Tot45Jaar_10", "k_45Tot65Jaar_11", "k_65JaarOfOuder_12",
    "GeboorteRelatief_25", "Gescheiden_15", "HuishoudensTotaal_28",
    "AantalInkomensontvangers_64", "GemiddeldInkomenPerInkomensontvanger_65",
    "GemiddeldInkomenPerInwoner_66", "k_65JaarOfOuder_12",
    "k_40PersonenMetLaagsteInkomen_67", "k_20PersonenMetHoogsteInkomen_68",
    "k_40HuishoudensMetLaagsteInkomen_70",
    "k_20HuishoudensMetHoogsteInkomen_71",
    "HuishoudensMetEenLaagInkomen_72"))

# view dataframe

View(data)

# probably rename columns

old_names <- c("AantalInwoners_5", "k_0Tot15Jaar_8", "k_15Tot25Jaar_9",
    "k_25Tot45Jaar_10", "k_45Tot65Jaar_11", "k_65JaarOfOuder_12",
     "GeboorteRelatief_25", "Gescheiden_15", "HuishoudensTotaal_28",
      "AantalInkomensontvangers_64", "GemiddeldInkomenPerInkomensontvanger_65",
    "GemiddeldInkomenPerInwoner_66", "k_65JaarOfOuder_12",
    "k_40PersonenMetLaagsteInkomen_67", "k_20PersonenMetHoogsteInkomen_68",
    "k_40HuishoudensMetLaagsteInkomen_70",
    "k_20HuishoudensMetHoogsteInkomen_71",
    "HuishoudensMetEenLaagInkomen_72")

new_names <- c("Aantal_Inwoners", "k0Tot15Jaar", "k15Tot25Jaar", "k25Tot45Jaar",
    "k45Tot65Jaar", "k65JaarOfOuder", "Geboorte_Relatief", "Gescheiden",
    "Huishoudens_Totaal", "Aantal_Inkomensontvangers",
    "Gemiddeld_Inkomen_Per_Inkomensontvanger", "Gemiddeld_Inkomen_Per_Inwoner",
    "k40procent_Personen_Met_Laagste_Inkomen",
    "k20procent_Personen_MetHoogste_Inkomen",
    "k40procent_Huishoudens_Met_Laagste_Inkomen",
    "k20procent_Huishoudens_Met_Hoogste_Inkomen",
    "Huishoudens_Met_Een_Laag_Inkomen")

data <- data  %>%
    dplyr::rename_at(all_of(old_names), ~ new_names)
    # all_of(old_names) instead of vars

# unprocessed Leistert df

Leistert_df <- fread(file = "CASE_1_LEISTERT_Final_with_Lifestyle.csv",
 sep = ";")

colnames(Leistert_df) <- gsub(" ", "_", colnames(Leistert_df))

# create month and year column

Leistert_df$Aankomst <- as.Date(Leistert_df$Aankomst)
Leistert_df$month <- strftime(Leistert_df$Aankomst, "%m")
Leistert_df$year <- strftime(Leistert_df$Aankomst, "%Y")

# descriptive statistics of cbs data

skim(data)

View(data)

# only work with data on Gemeente level

data$SoortRegio_2  <- as.character(data$SoortRegio_2)

data$SoortRegio_2 <- trimws(data$SoortRegio_2)

data_gemeente <- data  %>%
    filter(SoortRegio_2 == "Gemeente")

View(data_gemeente)

# drop non numerical columns

data_gemeente <- data_gemeente %>%
select(-Gemeentenaam_1, -SoortRegio_2)

# visualisations of variables

# without generating plots

plotHistFunc <- function(x, na.rm = TRUE) {
  nm <- names(x)
  for (i in seq_along(nm)) {
print(ggplot(x, aes_string(x = nm[i])) +
 geom_histogram(alpha = .5, fill = "mediumseagreen"))
 }
}

# function generates plots

plotHistFunc2 <- function(x, na.rm = TRUE) {
  nm <- names(x)
  for (i in seq_along(nm)) {
plots <- ggplot(x,aes_string(x = nm[i])) +
geom_histogram(alpha = .5, fill = "dodgerblue")
ggsave(plots, filename = paste("myplot", nm[i], ".png", sep = ""))
  }
}

# apply plotting function

plotHistFunc2(data_gemeente)

# get descriptives about variables

# ensure data data is stored as data.frame
# tibble is not working 
data_gemeente <- as.data.frame(data_gemeente)

skim(data_gemeente)

stargazer(data_gemeente, type = "text",
     digits = 1, title = "Descriptive statistics",
     digit.separator = ".", decimal.mark = ",")
# change type to "latex" to include in the report