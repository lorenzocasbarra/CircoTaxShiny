#' CircoTax Shiny UI Module
#'
#' This function defines the user interface for the CircoTax visualization
#' module. It provides a customizable circular taxonomy plot together with
#' interactive controls for settings, color palettes, filters, and annotations.
#'
#' The UI includes:
#' * Custom CSS (layout, plot area, control panels)
#' * JavaScript utilities for hiding panels when clicking outside
#' * A toolbar with action buttons for controlling plot settings
#' * Settings panels (hidden by default)
#' * A `plotOutput` for rendering the CircoTax plot
#' * A download button for exporting the plot as a PNG image
#'
#' @param id A module ID string used to namespace the UI and inputs.
#' @param disableSettings Logical (default `FALSE`). If set to `TRUE`, all
#'   interactive settings buttons (settings, palette, filters, annotations)
#'   are hidden and only the plot + download button are displayed.
#'
#' @return A `shiny::tagList()` representing the UI layout of the module.
#'
#' @seealso [CircoTaxServer()] for the corresponding server logic.
#'
#' @export
CircoTaxShinyUI <- function(id,	disableSettings=FALSE) {
	ns <- shiny::NS(id)
	shiny::fluidPage(
		shinyjs::useShinyjs(),
		shiny::tags$head(
			shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/body.css"),
			shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/circo.css"),
			shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/tableDiv.css"),
			shiny::tags$link(rel = "stylesheet", type = "text/css", href = "css/plotSettingsDiv.css")
		),
		
		shiny::tags$script(src = "js/hide.js"),

		shiny::div(
			id="circoDiv",
			shinyjs::hidden(
				shiny::textInput(
					inputId = shiny::NS(id,"onDisplay"),
					label = "onDisplay",
					value = 'none'
				)
			),
			shiny::div(
				id="circoSettingsDiv",
				{
					if (!disableSettings) {
						shiny::actionButton(
							inputId = shiny::NS(id,"showCircoSettings"),
							label = "",
							icon = shiny::icon("gear")
						)
					}
				},
				{
					if (!disableSettings) {
						shiny::actionButton(
							inputId = shiny::NS(id,"showCircoPalette"),
							label = "",
							icon = shiny::icon("palette")
						)
					}
				},
	
				{
					if (!disableSettings) {
						shiny::actionButton(
							inputId = shiny::NS(id,"showCircoFilters"),
							label = "",
							icon = shiny::icon("filter")
						)
					}
					
				},

				{
					if (!disableSettings) {
						shiny::actionButton(
							inputId = shiny::NS(id,"showCircoAnnotation"),
							label = "",
							icon = shiny::icon("note-sticky")
						)
					}
				},

				shiny::downloadButton(
					outputId = shiny::NS(id,"down"),
					label = "Download"
				)
			),
			
			shiny::div(
				id="circoContainer",
				shinyjs::hidden(
					CircoTaxDisplaySettings(id),
					CircoTaxDisplayAnnotation(id),
					CircoTaxDisplayPalette(id),
					CircoTaxDisplayFilters(id)
				),
				shiny::plotOutput(outputId = shiny::NS(id,"circo"),height = "fit-content"),
			)
		)

	)
}

#' CircoTax Shiny Server Module
#'
#' This function implements the server logic for the CircoTax visualization
#' module. It handles interactive input updates, dynamic UI display,
#' reactive extraction of user parameters, and rendering of the final
#' circular taxonomy plot via `circotax::CircoTax()`.
#'
#' ## Key Features
#' * Manages visibility of settings panels (settings, palette, filters,
#'   annotations) through `shinyjs`.
#' * Updates numeric inputs automatically according to the dimensions of
#'   the dataset.
#' * Supports two modes:
#'   - `'multi'`: a range of taxonomic columns (`tax_column_start:tax_column_end`)
#'   - `'single'`: name and taxon columns (`tax_column_start`, `tax_column_end`)
#' * Combines user inputs with optional externally supplied reactives
#'   (`titleText`, `size_taxon_circo`, `ramp`, etc.).
#' * Renders the circular taxonomy plot.
#' * Provides a download handler producing a high-resolution PNG image.
#'
#' @param input Standard Shiny module argument.
#' @param output Standard Shiny module argument.
#' @param session Standard Shiny module argument.
#' @param data A reactive expression returning a data.frame compatible with
#'   `circotax::CircoTax()`.
#' @param titleText Optional reactive providing a plot title.
#' @param size_taxon_circo Optional reactive specifying taxon size scaling.
#' @param fill_text Optional reactive specifying the text fill field.
#' @param ramp Optional reactive defining the color ramp.
#' @param fc_col Optional reactive indicating the fold-change (or value) column.
#' @param sort Optional reactive specifying sorting behavior.
#' @param collapse_ranks Optional reactive controlling taxonomic rank collapsing.
#' @param mode Optional reactive defining `'single'` or `'multi'` mode.
#' @param tax_column_start Optional reactive giving starting taxon column index.
#' @param tax_column_end Optional reactive giving ending taxon column index.
#' @param renderWidth,renderHeight Pixel dimensions for the generated plot.
#'
#' @return The function sets up output bindings for:
#' * `output$circo` – the rendered circular taxonomy plot
#' * `output$down` – a PNG file download handler
#'
#' @details
#' The server logic internally derives several reactive values (e.g.,
#' `modeToUse()`, `tax_col()`, `titleToUse()`, `rampToUse()`) that combine
#' user-supplied UI inputs with optional externally provided reactives.  
#' This allows the module to be used both interactively and programmatically.
#'
#' The PNG export uses `ragg::agg_png()` to ensure high-quality rendering
#' across platforms.
#'
#' @seealso [CircoTaxShinyUI()], [circotax::CircoTax()]
#'
#' @export
CircoTaxServer <- function(
	input,
	output,
	session,
	data,
	titleText=NULL,
	size_taxon_circo=NULL,
	fill_text=NULL,
	ramp=NULL,
	fc_col=NULL,
	sort=NULL,
	collapse_ranks=NULL,
	mode=NULL,
	tax_column_start=NULL,
	tax_column_end=NULL,
	renderWidth=1000,
	renderHeight=1000
) {	
	shinyjs::runjs("hideOnClickOutside(['circotax_ui-circoTableSettings','circotax_ui-circoAnnotationSettings', 'circotax_ui-circoPaletteSettings','circotax_ui-circoFiltersSettings']);")
	
	shiny::observeEvent(input$showCircoSettings,{
		print(session$ns)
		shinyjs::show("circoTableSettings")
	})
	
	shiny::observeEvent(input$showCircoAnnotation,{
		shinyjs::show("circoAnnotationSettings")
	})
	shiny::observeEvent(input$showCircoPalette,{
		shinyjs::show("circoPaletteSettings")
	})
	shiny::observeEvent(input$showCircoFilters,{
		shinyjs::show("circoFiltersSettings")
	})

	shiny::observe({
		shiny::req(data())
		max_val<-ncol(data())
		shiny::updateNumericInput(
			session,
			inputId = "fc_col",
			max = max_val
		)
		shiny::updateNumericInput(
			session,
			inputId = "tax_column_start",
			max = max_val
		)
		shiny::updateNumericInput(
			session,
			inputId = "tax_column_end",
			max = max_val
		)
	})
	
	modeToUse <- shiny::reactive({
		if (!is.null(mode)) {
			mode()
		} else {
			input$mode_select
		}
	})

	shiny::observeEvent(input$mode_select,{
		if (input$mode_select == "multi") {
			shiny::updateNumericInput(
				session,
				inputId = "fc_col",
			)
			shiny::updateNumericInput(
				session,
				inputId = "tax_column_start",
				label = "Taxon Column Start Index",
			)
			shiny::updateNumericInput(
				session,
				inputId = "tax_column_end",
				label = "Taxon Column End Index",
			)
		} else {
			shiny::updateNumericInput(
				session,
				inputId = "fc_col",
			)
			shiny::updateNumericInput(
				session,
				inputId = "tax_column_start",
				label = "Names",
			)
			shiny::updateNumericInput(
				session,
				inputId = "tax_column_end",
				label = "Taxon",
			)
		}
	})

	taxStart <- shiny::reactive({
		if (!is.null(tax_column_start)) {
			tax_column_start()
		} else {
			input$tax_column_start
		}
	})
	taxEnd <- shiny::reactive({
		if (!is.null(tax_column_end)) {
			tax_column_end()
		} else {
			input$tax_column_end
		}
	})

	tax_col <- shiny::reactive({
		if (modeToUse() == "multi") {
			taxStart():taxEnd()
		} else {
			taxEnd()
		}
	})

	titleToUse <- shiny::reactive({
		if (!is.null(titleText)) {
			titleText()
		} else {
			input$title
		}
	})
	taxonSizeToUse <- shiny::reactive({
		if (!is.null(size_taxon_circo)) {
			size_taxon_circo()
		} else {
			input$size_taxon_circo
		}
	})
	fillTextToUse <- shiny::reactive({
		if (!is.null(fill_text)) {
			fill_text()
		} else {
			input$fill_text
		}
	})
	rampToUse  <- shiny::reactive({
		if (!is.null(ramp)) {
			ramp()
		} else {
			c(input$color1,input$color2,input$color3)
		}
	})
	fcToUse <- shiny::reactive({
		if (!is.null(fc_col)) {
			fc_col()
		} else {
			input$fc_col
		}
	})

	sortToUse <- shiny::reactive({
		if (!is.null(sort)) {
			sort()
		} else {
			input$sort
		}
	})
	collapseToUse <- shiny::reactive({
		if (!is.null(collapse_ranks)) {
			collapse_ranks()
		} else {
			input$collapse_ranks
		}
	})

  output$circo <- shiny::renderPlot(
		width = renderWidth, height = renderHeight,{
		# req(data())
		circotax::CircoTax(
			data(),
			title = titleToUse(),
			fill_text = fillTextToUse(),
			fc_col = fcToUse(),
			size_taxon_circo = taxonSizeToUse(),
			sort = sortToUse(),
			tax_col = tax_col(),
			collapse_ranks = collapseToUse(),
			ramp=rampToUse(),
			names=taxStart()
		)
  })
	output$down <- shiny::downloadHandler(
		filename =  function() {
			paste0("circotax_plot_", Sys.Date(), ".png")
		},
		content = function(file) {
			ragg::agg_png(file, width = 4000, height = 4000, units = "px", res = 300, scaling = 1)
				circotax::CircoTax(
					data(),
					title = titleToUse(),
					fill_text = fillTextToUse(),
					fc_col = fcToUse(),
					size_taxon_circo = taxonSizeToUse(),
					sort = sortToUse(),
					tax_col = tax_col(),
					collapse_ranks = collapseToUse(),
					ramp=rampToUse(),
					names=taxStart()
				)
			dev.off()
		} 
	)
}
