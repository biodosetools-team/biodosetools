# Load packages
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(plotly)

# Locale ----

countries <- c(
  "en" = "Englsih",
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
