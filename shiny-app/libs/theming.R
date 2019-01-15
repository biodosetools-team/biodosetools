# Load functions -------------------------------------------

source("libs/theming_functions.R")

# Custom logo ----------------------------------------------

app_name <- c(
  "Biodose",
  "Tools"
)
app_version <- "ALPHA"

logo_biodose <- shinyDashboardAltLogoDIY(

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

# Status colors
options_color <- "#2B7C9A"
inputs_color <-  "#5B5EA8"
results_color <- main_color
export_color <-  "#F6A945"

# Box colors
# primary_color <- "rgb(15,124,191)"
# success_color <- "rgb(59,133,95)"
# warning_color <- "rgb(178,83,149)"
# danger_color <-  "rgb(207,57,92)"
primary_color <- inputs_color
success_color <- results_color
warning_color <- options_color
danger_color <-  export_color

# TabBox colors
# tabbox_tabtext_color <-          "rgb(42,102,98)"
# tabbox_tabtext_selected_color <- "rgb(207,57,92)"
# tabbox_back_color <-             "rgb(248,248,248)"
# tabbox_highlight_color <-        "rgb(207,57,92)"
tabbox_tabtext_color <-          "black"
tabbox_tabtext_selected_color <- "black"
tabbox_back_color <-             "rgb(248,248,248)"
tabbox_highlight_color <-        results_color

theme_biodose <- shinyDashboardThemeDIY(

  # General ----
  appFontFamily = "Roboto",
  appFontColor = "black",
  bodyFontSize = 16,
  bodyBackColor = "white",

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
  sidebarBackColor = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgb(241,241,241)",
    colorMiddle = "rgb(237,237,237)",
    colorEnd = "rgb(210,210,210)",
    colorStartPos = 0,
    colorMiddlePos = 97,
    colorEndPos = 100
  ),
  sidebarPadding = 0,
  sidebarMenuBackColor = "transparent",
  sidebarMenuPadding = 0,
  sidebarMenuBorderRadius = 0,
  sidebarShadowRadius = "0px 0px 0px",
  sidebarShadowColor = "",
  sidebarUserTextColor = "black",
  sidebarSearchBackColor = "white",
  sidebarSearchIconColor = head_color_alt,
  sidebarSearchBorderColor = "rgb(210,210,210)",
  sidebarTabTextColor = "black",
  sidebarTabTextSize = 14,
  sidebarTabBorderStyle = "none",
  sidebarTabBorderColor = "",
  sidebarTabBorderWidth = 0,
  sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "down",
    colorStart = "rgb(193,193,193)",
    colorMiddle = "rgb(216,216,216)",
    colorEnd = "rgb(218,218,218)",
    colorStartPos = 0,
    colorMiddlePos = 5,
    colorEndPos = 100
  ),
  sidebarTabTextColorSelected = accent_color,
  sidebarTabRadiusSelected = "0px",
  sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right",
    colorStart = "rgb(230,230,230)",
    colorMiddle = "rgb(225,225,225)",
    colorEnd = "rgb(210,210,210)",
    colorStartPos = 0,
    colorMiddlePos = 97,
    colorEndPos = 100
  ),
  sidebarTabTextColorHover = "black",
  sidebarTabBorderStyleHover = "none",
  sidebarTabBorderColorHover = "",
  sidebarTabBorderWidthHover = 0,
  sidebarTabRadiusHover = "0px",

  # Boxes ----
  boxBackColor = "rgb(248,248,248)",
  boxBorderRadius = 3,
  boxShadowSize = "0px 0px 0px",
  boxShadowColor = "",
  boxTitleSize = 18,
  boxDefaultColor = "rgb(248,248,248)",
  boxPrimaryColor = primary_color,
  boxSuccessColor = success_color,
  boxWarningColor = warning_color,
  boxDangerColor = danger_color,
  boxTopBorder = 6,

  # tabBoxes ----
  tabBoxTabColor = "rgb(248,248,248)",
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
