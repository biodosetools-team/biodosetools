# Libraries ------------------------------------------------

library(shiny.i18n)


# Locales --------------------------------------------------

countries <- c(
  "en" = "English",
  "es" = "Spanish",
  "de" = "German",
  "fr" = "French"
)

flags <- c(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/gb.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/es.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/de.svg",
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/fr.svg"
)


# i18n objects ---------------------------------------------

# File with translations
i18n <- Translator$new(translation_csvs_path = "translations")
# Set language
i18n$set_translation_language("it")
