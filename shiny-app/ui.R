# Libraries ------------------------------------------------

# UI Libs
library(shinydashboard)
library(dygraphs)
library(shinyjs)


# Header ---------------------------------------------------

header <- dashboardHeader(
	title = span(tagList(icon("calculator"), "Biodose Tool"))
	# title = "Biodose Tool"
)

plot.checkbox <- checkboxGroupInput(
	"plots_checkbox", "Plots",
	choices = c(
		"Time Series" = "time_series",
		"Phase Portrait" = "phase_port"
		# "3D Trajectory" = "3d_traj"
	)
)

# Model A Widgets ####
plot.inputs.rep <- box(
	width = 12,
	sliderInput("slider_r1", label = "R1", min = 0, max = 1, step = 0.1, value = 1)
)

plot.params.rep1 <- box(
	width = 12,
	sliderInput("slider_kappa",  label = "Kappa", min = 0, max = 1, step = 0.1, value = 1),
	sliderInput("slider_gamma", label = "Gamma", min = 0, max = 1, step = 0.1, value = 1)
)

plot.params.rep2 <- box(
	width = 12,
	sliderInput("slider_alpha",  label = "Omega", min = 0, max = 1, step = 0.1, value = 0.1),
	sliderInput("slider_sigma",  label = "Sigma", min = 0, max = 1, step = 0.1, value = 0.1)
)

# Model B Widgets ####
plot.inputs.bip <- box(
	width = 12,
	sliderInput("slider_r1_bi", label = "R1", min = 0, max = 1, step = 0.1, value = 1),
	sliderInput("slider_r2_bi", label = "R2", min = 0, max = 1, step = 0.1, value = 0.5)
)

plot.params.bip1 <- box(
	width = 12,
	sliderInput("slider_kappa_bi",   label = "Kappa",   min = 0, max = 1, step = 0.1, value = 1),
	sliderInput("slider_alpha_bi",   label = "Alpha",   min = 0, max = 1, step = 0.1, value = 1)
)

plot.params.bip2 <- box(
	width = 12,
	sliderInput("slider_gamma_bi",   label = "Gamma",   min = 0, max = 1, step = 0.05, value = 0.1),
	sliderInput("slider_sigma_bi",   label = "Sigma",   min = 0, max = 1, step = 0.1, value = 0.1)
)

plot.params.bip3 <- box(
	width = 12,
	sliderInput("slider_epsilon_bi", label = "Epsilon", min = 0, max = 1, step = 0.1, value = 0.1),
	sliderInput("slider_delta_bi",   label = "Delta",   min = 0, max = 1, step = 0.1, value = 0.1)
)

# Tripartite model ####
plot.inputs.tri <- box(
	width = 12,
	sliderInput("slider_r1_tri", label = "R1", min = 0, max = 1, step = 0.1, value = 1),
	sliderInput("slider_r2_tri", label = "R2", min = 0, max = 1, step = 0.1, value = 0.5),
	sliderInput("slider_r3_tri", label = "R3", min = 0, max = 1, step = 0.1, value = 0.5)
)

plot.params.tri1 <- box(
	width = 12,
	sliderInput("slider_kappa_tri",   label = "Kappa",   min = 0, max = 1, step = 0.1, value = 1),
	sliderInput("slider_alpha_tri",   label = "Alpha",   min = 0, max = 1, step = 0.1, value = 1),
	sliderInput("slider_omega_tri",   label = "Omega",   min = 0, max = 1, step = 0.1, value = 1)
)

plot.params.tri2 <- box(
	width = 12,
	sliderInput("slider_mu_tri",   label = "Mu",   min = 0, max = 1, step = 0.1, value = 1),
	sliderInput("slider_beta_tri",   label = "Beta",   min = 0, max = 1, step = 0.1, value = 0.6),
	sliderInput("slider_gamma_tri",   label = "Gamma",   min = 0, max = 1, step = 0.05, value = 0.1)
)

plot.params.tri3 <- box(
	width = 12,
	sliderInput("slider_sigma_tri",   label = "Sigma",   min = 0, max = 1, step = 0.1, value = 0.1),
	sliderInput("slider_epsilon_tri", label = "Epsilon", min = 0, max = 1, step = 0.1, value = 0.1),
	sliderInput("slider_delta_tri",   label = "Delta",   min = 0, max = 1, step = 0.1, value = 0.1)
)

# Sidebar --------------------------------------------------

sidebar <- dashboardSidebar(
	sidebarMenu(
		id = "sidebarmenu",
		menuItem("About this App", tabName = "home",  icon = icon("home"), selected = T),
		menuItem("First Model", tabName = "model-a",  icon = icon("circle")),
		menuItem("Second Model", tabName = "model-b", icon = icon("square")),

		plot.checkbox
	)
)


# Body -----------------------------------------------------

body <- dashboardBody(
	useShinyjs(),

	tags$head(tags$style(
		HTML('
			.content-wrapper,
			.right-side {
				background-color: #ffffff;
			}
				 '))),

	tabItems(
		tabItem(tabName = "home",
						h2("About this project"),
						includeMarkdown("body.md"),
						actionButton(
							inputId='ab1', label="Fork this project",
							icon = icon("github"),
							onclick ="window.open(
							'https://github.com/biodosimetry-uab/biodose-tool', '_blank')"
							)
		),

		# Model A ####
		tabItem(tabName = "model-a",
						h2("Model A"),
						fluidRow(
							column(width = 4,
										 h4("Input"),
										 plot.inputs.rep
							),
							column(width = 8,
										 h4("Parameters"),
										 fluidRow(
										 	column(width = 6,
										 				 plot.params.rep1
										 	),
										 	column(width = 6,
										 				 plot.params.rep2
										 	)
										 )
							)
						)
		),

		# Model B ####
		tabItem(tabName = "model-b",
						h2("Model B"),
						fluidRow(
							column(width = 3,
										 h4("Input"),
										 plot.inputs.bip
							),
							column(width = 9,
										 h4("Parameters"),
										 fluidRow(
										 	column(width = 4,
										 				 plot.params.bip1
										 	),
										 	column(width = 4,
										 				 plot.params.bip2
										 	),
										 	column(width = 4,
										 				 plot.params.bip3
										 	)
										 )
							)
						)
		)

	),

	fluidRow(
		column(width = 12,
					 uiOutput('ui_plots')
					 )
	)
)


# Build UI -------------------------------------------------

ui <- dashboardPage(
	skin = "green",
	header,
	sidebar,
	body
)
