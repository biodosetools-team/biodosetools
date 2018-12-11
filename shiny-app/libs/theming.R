# Load functions -------------------------------------------

source("libs/theming_functions.R")

# Custom logo ----------------------------------------------

app_name <- c(
  "Biodose",
  "Tools"
)
app_version <- "ALPHA"

logo_biodose <- shinyDashboardAltLogoDIY(
  icon = "icon_small.png",
  fontFamily = "Roboto",
  boldText = app_name[[1]],
  mainText = app_name[[2]],
  textSize = 18,
  badgeText = app_version,
  badgeTextColor = "white",
  badgeTextSize = 2,
  badgeBackColor = "#311B92",
  badgeBorderRadius = 3
)


# Custom theme ---------------------------------------------

head_color <- "rgb(46,194,126)"
head_color_alt <- "rgb(91,94,168)"
accent_color <- "rgb(91,94,168)"

theme_biodose <- shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Roboto",
  appFontColor = "rgb(0,0,0)",
  bodyFontSize = 16,
  bodyBackColor = "rgb(255,255,255)",

  ### header
  logoBackColor = head_color,
  headerButtonBackColor = head_color_alt,
  headerButtonIconColor = "rgb(255,255,255)",
  headerButtonBackColorHover = "rgb(58,61,141)",
  headerButtonIconColorHover = "rgb(0,0,0)",
  headerBackColor = head_color_alt,
  headerBoxShadowColor = "rgb(220,220,220)",
  headerBoxShadowSize = "2px 3px 2px",

  ### sidebar
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
  sidebarUserTextColor = "rgb(0,0,0)",
  sidebarSearchBackColor = "rgb(255,255,255)",
  sidebarSearchIconColor = head_color_alt,
  sidebarSearchBorderColor = "rgb(210,210,210)",
  sidebarTabTextColor = "rgb(0,0,0)",
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
  sidebarTabTextColorHover = "rgb(0,0,0)",
  sidebarTabBorderStyleHover = "none",
  sidebarTabBorderColorHover = "",
  sidebarTabBorderWidthHover = 0,
  sidebarTabRadiusHover = "0px",

  ### boxes
  boxBackColor = "rgb(248,248,248)",
  boxBorderRadius = 0,
  boxShadowSize = "0px 0px 0px",
  boxShadowColor = "",
  boxTitleSize = 18,
  boxDefaultColor = "rgb(248,248,248)",
  boxPrimaryColor = "rgb(15,124,191)",
  boxSuccessColor = "rgb(59,133,95)",
  boxWarningColor = "rgb(178,83,149)",
  boxDangerColor = "rgb(207,57,92)",
  tabBoxTabColor = "rgb(248,248,248)",
  tabBoxTabTextSize = 14,
  tabBoxTabTextColor = "rgb(42,102,98)",
  tabBoxTabTextColorSelected = "rgb(207,57,92)",
  tabBoxBackColor = "rgb(248,248,248)",
  tabBoxHighlightColor = "rgb(207,57,92)",
  tabBoxBorderRadius = 0,

  ### inputs
  buttonBackColor = "rgb(240,240,240)",
  buttonTextColor = "rgb(80,80,80)",
  buttonBorderColor = "rgb(185,185,185)",
  buttonBorderRadius = 5,
  buttonBackColorHover = "rgb(227,227,227)",
  buttonTextColorHover = "rgb(80,80,80)",
  buttonBorderColorHover = "rgb(210,210,210)",
  textboxBackColor = "rgb(255,255,255)",
  textboxBorderColor = "rgb(210,210,210)",
  textboxBorderRadius = 0,
  textboxBackColorSelect = "rgb(255,255,255)",
  textboxBorderColorSelect = "rgb(210,210,210)",

  ### tables
  tableBackColor = "rgb(255,255,255)",
  tableBorderColor = "rgb(235,235,235)",
  tableBorderTopSize = 1,
  tableBorderRowSize = 1
)
