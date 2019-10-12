# Libraries ------------------------------------------------

library(shiny.i18n)


# Locales --------------------------------------------------

countries <- c(
  "gb" = "English"#,
  # "es" = "Spanish",
  # "de" = "German",
  # "fr" = "French"
)

flags <- paste(
  "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/",
  names(countries),
  ".svg",
  sep = ""
)


# i18n objects ---------------------------------------------

# File with translations
i18n <- Translator$new(translation_csvs_path = "translations")
# Set language
i18n$set_translation_language("en")
