

CircoTaxDisplaySettings <- function(id) {
	shiny::div(
		id=shiny::NS(id,"circoTableSettings"),
		class = "circo-settings-display",
		shiny::numericInput(
			inputId = shiny::NS(id,"fc_col"),
			label = "FC Column Index",
			value = 1,
			min = 1,
			max = 7,
			step = 1
		),
		shiny::numericInput(
			inputId = shiny::NS(id,"tax_column_start"),
			label = "Taxon Column Start Index",
			value = 2,
			min = 1,
			max = 99,
			step = 1
		),
		shiny::numericInput(
			inputId = shiny::NS(id,"tax_column_end"),
			label = "Taxon Column End Index",
			value = 7,
			min = 1,
			max = 99,
			step = 1
		),
		shiny::radioButtons(
      shiny::NS(id,"mode_select"),
      label = "Dataset Type",
      selected = "multi",
      choiceNames = c("mono column","multi column"),
      choiceValues = c("mono","multi")
    )
	)
}
CircoTaxDisplayPalette <- function(id) {
	shiny::div(
		id=shiny::NS(id,"circoPaletteSettings"),
		class = "circo-settings-display",
		colourpicker::colourInput(shiny::NS(id,"color1"), "Select colour (min)", "orange"),
		colourpicker::colourInput(shiny::NS(id,"color2"), "Select colour (mid)", "white"),
		colourpicker::colourInput(shiny::NS(id,"color3"), "Select colour (max)", "blue"),
	)
}
CircoTaxDisplayFilters <- function(id) {
	shiny::div(
		id=shiny::NS(id,"circoFiltersSettings"),
		class = "circo-settings-display",
		shiny::selectInput(
			inputId = shiny::NS(id,"sort"),
			label = "Sort by",
			choices = c("no","rank","fc","absfc","alpha","alpharank"),
			selected = "rank"
		),
		shiny::checkboxInput(shiny::NS(id,"collapse_ranks"),"Collapse Ranks",FALSE)
	)
}
CircoTaxDisplayAnnotation <- function(id) {
	shiny::div(
		id=shiny::NS(id,"circoAnnotationSettings"),
		class = "circo-settings-display",
		shiny::textInput(
			inputId = shiny::NS(id,"title"),
			label = "Plot Title",
			value = "CircoTax plot"
		),
		shiny::textInput(
			inputId = shiny::NS(id,"fill_text"),
			label = "Legend Title",
			value = "logFC"
		),
		shiny::numericInput(
			inputId = shiny::NS(id,"size_taxon_circo"),
			label = "Taxon Size",
			value = 3,
			min = 1,
			max = 99,
			step = 1
		),
	)
}
