# Load functions -------------------------------------------

source("libs/theming_functions.R")

# Custom logo ----------------------------------------------

app_name <- c(
  "Biodose",
  "Tools"
)
app_version <- "ALPHA"

logo_biodose_tools <- shinyDashboardAltLogoDIY(

  # Icon ----
  icon = "icon_small.svg",

  # Text ----
  fontFamily = "Roboto",
  boldText = app_name[[1]],
  mainText = app_name[[2]],
  textSize = 18,

  # Badge ----
  badgeText = app_version,
  badgeTextColor = "white",
  badgeTextSize = 2,
  badgeBackColor = "#311B92",
  badgeBorderRadius = 3
)


# Custom theme ---------------------------------------------

# Custom colors
head_color <-     "#2EC27E"
head_color_alt <- "#5B5EA8"
accent_color <-   "#311B92"
main_color <-     "#2EC27E"

# Home colors
home_color =           "#6C63FF"
home_color_border =    "#514bc0"
home_color_hover =     "#8b84ff"

# Options colors
options_color =        "#2B7C9A"
options_color_border = "#26618a"
options_color_hover =  "#27a1d0"

# Inputs colors
inputs_color =         "#5B5EA8"
inputs_color_border =  "#4c44ac"
inputs_color_hover =   "#7275ce"

# Results colors
results_color =        main_color
results_color_border = "#13794a"
results_color_hover =  "#37e495"

# Export colors
export_color =         "#F6A945"
export_color_border =  "#eb8a0b"
export_color_hover =   "#ffba63"

# Box colors
# primary_color <- "rgb(15,124,191)"
# success_color <- "rgb(59,133,95)"
# warning_color <- "rgb(178,83,149)"
# danger_color <-  "rgb(207,57,92)"
primary_color <- inputs_color
success_color <- results_color
warning_color <- options_color
danger_color <-  export_color

box_back_color <- "rgb(255,255,255)"

# TabBox colors
# tabbox_tabtext_color <-          "rgb(42,102,98)"
# tabbox_tabtext_selected_color <- "rgb(207,57,92)"
# tabbox_back_color <-             "rgb(248,248,248)"
# tabbox_highlight_color <-        "rgb(207,57,92)"
tabbox_tabtext_color <-          "black"
tabbox_tabtext_selected_color <- "black"
tabbox_back_color <-             box_back_color
tabbox_highlight_color <-        results_color


# Theme ----------------------------------------------------

theme_biodose_tools <- shinyDashboardThemeDIY(

  # General ----
  appFontFamily = "Roboto",
  appFontColor = "black",
  bodyFontSize = 16,
  # bodyBackColor = "#F4F6F9",
  bodyBackColor = "#F7FAFC",

  # Header ----
  logoBackColor = head_color,
  headerButtonBackColor = head_color_alt,
  headerButtonIconColor = "white",
  headerButtonBackColorHover = "rgb(58,61,141)",
  headerButtonIconColorHover = "black",
  headerBackColor = head_color_alt,
  headerBoxShadowColor = "rgb(220,220,220)",
  headerBoxShadowSize = "0px 3px 2px",

  # Sidebar ----
  sidebarBackColor = "rgb(255,255,255)",
  sidebarPadding = "",
  sidebarMenuBackColor = "transparent",
  sidebarMenuPadding = 30,
  sidebarMenuBorderRadius = 0,
  # sidebarShadowRadius = "0 10px 10px",
  # sidebarShadowColor = "rgba(0,0,0,.39)",
  sidebarShadowRadius = "0 0 36px 0",
  sidebarShadowColor = "rgba(136,152,170,.15)",
  sidebarUserTextColor = "black",
  sidebarSearchBackColor = "white",
  sidebarSearchIconColor = head_color_alt,
  sidebarSearchBorderColor = "rgb(210,210,210)",

  sidebarTabTextColor = "black",
  sidebarTabTextSize = 14,
  sidebarTabBorderStyle = "none",
  sidebarTabBorderColor = "",
  sidebarTabBorderWidth = 0,

  sidebarTabBackColorSelected = "#5B5EA8",
  sidebarTabTextColorSelected = "rgb(255,255,255)",
  sidebarTabRadiusSelected = "5px",

  sidebarTabBackColorHover = "rgb(234,234,234)",
  sidebarTabTextColorHover = "rgb(0,0,0)",
  sidebarTabBorderStyleHover = "none",
  sidebarTabBorderColorHover = "none",
  sidebarTabBorderWidthHover = 0,
  sidebarTabRadiusHover = "5px",

  # Boxes ----
  boxBackColor = box_back_color,
  boxBorderRadius = 5,
  boxShadowSize = "0 1px 5px",
  boxShadowColor = "rgba(0, 0, 0, .1)",
  boxTitleSize = 18,
  boxDefaultColor = box_back_color,
  boxPrimaryColor = primary_color,
  boxSuccessColor = success_color,
  boxWarningColor = warning_color,
  boxDangerColor = danger_color,
  boxTopBorder = 6,

  # tabBoxes ----
  tabBoxTabColor = tabbox_back_color,
  tabBoxTabTextSize = 14,
  tabBoxTabTextColor = tabbox_tabtext_color,
  tabBoxTabTextColorSelected = tabbox_tabtext_selected_color,
  tabBoxBackColor = tabbox_back_color,
  tabBoxHighlightColor =  tabbox_highlight_color,
  tabBoxBorderRadius = 3,
  tabBoxTopBorder = 4,

  # Inputs ----
  buttonBackColor = "rgb(240,240,240)",
  buttonTextColor = "rgb(80,80,80)",
  buttonBorderColor = "rgb(185,185,185)",
  buttonBorderRadius = 3,
  buttonBackColorHover = "rgb(227,227,227)",
  buttonTextColorHover = "rgb(80,80,80)",
  buttonBorderColorHover = "rgb(210,210,210)",
  textboxBackColor = "white",
  textboxBorderColor = "rgb(210,210,210)",
  textboxBorderRadius = 3,
  textboxBackColorSelect = "white",
  textboxBorderColorSelect = "rgb(210,210,210)",

  # Tables ----
  tableBackColor = "white",
  tableBorderColor = "rgb(235,235,235)",
  tableBorderTopSize = 1,
  tableBorderRowSize = 1
)


# Theme extras ---------------------------------------------

theme_buttons_biodose_tools <- shinyDashboardButtonsStatus(
  # Home colors
  home_color = home_color,
  home_color_border = home_color_border,
  home_color_hover = home_color_hover,

  # Options colors
  options_color = options_color,
  options_color_border = options_color_border,
  options_color_hover = options_color_hover,

  # Inputs colors
  inputs_color = inputs_color,
  inputs_color_border = inputs_color_border,
  inputs_color_hover = inputs_color_hover,

  # Results colors
  results_color = results_color,
  results_color_border = results_color_border,
  results_color_hover = results_color_hover,

  # Export colors
  export_color = export_color,
  export_color_border = export_color_border,
  export_color_hover = export_color_hover
)
