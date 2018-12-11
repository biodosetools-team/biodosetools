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
  icon = "icon_small.png",

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

head_color <- "rgb(46,194,126)"
head_color_alt <- "rgb(91,94,168)"
accent_color <- "rgb(49,27,146)"
main_color <- "rgb(46,194,126)"

# Standard colors
black_color <- "rgb(0,0,0)"
white_color <- "rgb(255,255,255)"

theme_biodose <- shinyDashboardThemeDIY(

  # General ----
  appFontFamily = "Roboto",
  appFontColor = black_color,
  bodyFontSize = 16,
  bodyBackColor = white_color,

  # Header ----
  logoBackColor = head_color,
  headerButtonBackColor = head_color_alt,
  headerButtonIconColor = white_color,
  headerButtonBackColorHover = "rgb(58,61,141)",
  headerButtonIconColorHover = black_color,
  headerBackColor = head_color_alt,
  headerBoxShadowColor = "rgb(220,220,220)",
  headerBoxShadowSize = "2px 3px 2px",

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
  sidebarUserTextColor = black_color,
  sidebarSearchBackColor = white_color,
  sidebarSearchIconColor = head_color_alt,
  sidebarSearchBorderColor = "rgb(210,210,210)",
  sidebarTabTextColor = black_color,
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
  sidebarTabTextColorHover = black_color,
  sidebarTabBorderStyleHover = "none",
  sidebarTabBorderColorHover = "",
  sidebarTabBorderWidthHover = 0,
  sidebarTabRadiusHover = "0px",

  # Boxes ----
  boxBackColor = "rgb(248,248,248)",
  boxBorderRadius = 0,
  boxShadowSize = "0px 0px 0px",
  boxShadowColor = "",
  boxTitleSize = 18,
  boxDefaultColor = "rgb(248,248,248)",
  # boxPrimaryColor = "rgb(15,124,191)",
  boxPrimaryColor = accent_color,
  boxSuccessColor = "rgb(59,133,95)",
  boxWarningColor = "rgb(178,83,149)",
  boxDangerColor = "rgb(207,57,92)",
  tabBoxTabColor = "rgb(248,248,248)",
  tabBoxTabTextSize = 14,
  # tabBoxTabTextColor = "rgb(42,102,98)",
  tabBoxTabTextColor = black_color,
  # tabBoxTabTextColorSelected = "rgb(207,57,92)",
  tabBoxTabTextColorSelected = black_color,
  tabBoxBackColor = "rgb(248,248,248)",
  # tabBoxHighlightColor = "rgb(207,57,92)",
  tabBoxHighlightColor = main_color,
  tabBoxBorderRadius = 0,

  # Inputs ----
  buttonBackColor = "rgb(240,240,240)",
  buttonTextColor = "rgb(80,80,80)",
  buttonBorderColor = "rgb(185,185,185)",
  buttonBorderRadius = 5,
  buttonBackColorHover = "rgb(227,227,227)",
  buttonTextColorHover = "rgb(80,80,80)",
  buttonBorderColorHover = "rgb(210,210,210)",
  textboxBackColor = white_color,
  textboxBorderColor = "rgb(210,210,210)",
  textboxBorderRadius = 0,
  textboxBackColorSelect = white_color,
  textboxBorderColorSelect = "rgb(210,210,210)",

  # Tables ----
  tableBackColor = white_color,
  tableBorderColor = "rgb(235,235,235)",
  tableBorderTopSize = 1,
  tableBorderRowSize = 1
)
