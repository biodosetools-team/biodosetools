# Load functions -------------------------------------------

source("libs/theming_functions.R")

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



# Button theming -------------------------------------------

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
