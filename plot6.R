# plot6.R
# PM2.5 emissions comparison of motor vehicle related sources between Baltimore and Los Angeles, over the period 1999-2008.
# Course: Exploratory Data Analysis.
# Data Science Specialization.
# Second course project.
# Author: Pablo César Pérez González (pcesarperez@gmail.com)


# Constants.
#
# * `DATA_TABLE_PACKAGE`: Name of the `data.table` package, used in data transformation.
# * `GGPLOT2_PACKAGE`: Name of the `ggplot2` package, used to plot the data.
# * `GRID_PACKAGE`: Name of the `grid` package, used to create units for the plot margins.
# * `EMISSIONS_DATA_VARIABLE`: Name of the variable wich holds the emissions data.
# * `SOURCES_DATA_VARIABLE`: Name of the variable which holds the different sources for the emissions.
# * `DATA_FILE`: PM2.5 emissions data filename.
# * `SOURCES_FILE`: Emissions sources data filename.
# * `PNG_PLOT_FILE`: Filename of the PNG file which will hold the plot.
# * `PNG_WIDTH`: Width of the PNG file.
# * `PNG_HEIGHT`: Height of the PNG file.
# * `PLOT_TITLE`: Title of the plot.
# * `PLOT_TYPE`: Graph type (barchart).
# * `STAT_TYPE`: Statistics to use in the plot.
# * `POSITION_ADJUSTMENT`: Position adjustment to use in the plot.
# * `PLOT_X_LABEL`: Label of the X axis.
# * `PLOT_Y_LABEL`: Label of the Y axis.
# * `PLOT_COLOR_PALETTE`: Color palette used in the plot.
# * `PLOT_MARGIN_TOP`: Top margin of the graph.
# * `PLOT_MARGIN_BOTTOM`: Bottom margin of the graph.
# * `PLOT_MARGIN_LEFT`: Left margin of the graph.
# * `PLOT_MARGIN_RIGHT`: Right margin of the graph.
# * `PLOT_MARGIN_UNITS`: Units in which the margins are defined.
# * `MOTOR_VEHICLE_SOURCES_PATTERN`: Regular expression pattern for motor vehicles related sources.
# * `BALTIMORE_CODE`: Federal Information Processing Standard code for Baltimore City, Maryland.
# * `LOS_ANGELES_CODE`: Federal Information Processing Standard code for Los Angeles County, California.
# * `BALTIMORE_NAME`: Baltimore City full name, used in the plot faceting.
# * `LOS_ANGELES_NAME`: Los Angeles County full name, used in the plot faceting.
# * `EXTRANEOUS_SOURCE`: Emission source included by mistake in the first filter of the sources.
# * `SCALING_FACTOR`: Scaling factor to apply to the emissions.
# * `SLOPE_TEXT`: Text used to show the slope of each regression line.
DATA_TABLE_PACKAGE <- "data.table"
GGPLOT2_PACKAGE <- "ggplot2"
GRID_PACKAGE <- "grid"
EMISSIONS_DATA_VARIABLE <- "emissions_data"
SOURCES_DATA_VARIABLE <- "emissions_sources"
DATA_FILE <- "summarySCC_PM25.rds"
SOURCES_FILE <- "Source_Classification_Code.rds"
PNG_PLOT_FILE <- "plot6.png"
PNG_WIDTH <- 512
PNG_HEIGHT <- 512
PLOT_TITLE <- expression (atop ("PM"[2.5] ~ "emissions comparison of motor vehicle related sources", "in Baltimore City and Los Angeles County"))
PLOT_TYPE <- "point"
STAT_TYPE <- "identity"
POSITION_ADJUSTMENT <- "dodge"
PLOT_X_LABEL <- "Year"
PLOT_Y_LABEL <- "Emissions (ktons)"
PLOT_COLOR_PALETTE <- "Set1"
PLOT_MARGIN_TOP <- 15
PLOT_MARGIN_BOTTOM <- 5
PLOT_MARGIN_LEFT <- 5
PLOT_MARGIN_RIGHT <- 5
PLOT_MARGIN_UNITS <- "mm"
MOTOR_VEHICLE_SOURCES_PATTERN <- "(gasoline|diesel)"
BALTIMORE_CODE <- "24510"
LOS_ANGELES_CODE <- "06037"
BALTIMORE_NAME <- "Baltimore City, Maryland"
LOS_ANGELES_NAME <- "Los Angeles County, California"
EXTRANEOUS_SOURCE <- "Bulk Gasoline Terminals"
SCALING_FACTOR <- 1000
SLOPE_TEXT <- "Absolute slope:"


# The following packages need to be installed:
#
# * `data.table`
# * `ggplot2
if (!DATA_TABLE_PACKAGE %in% installed.packages ( )) {
	install.packages (DATA_TABLE_PACKAGE)
}
if (!GGPLOT2_PACKAGE %in% installed.packages ( )) {
	install.packages (GGPLOT2_PACKAGE)
}
if (!GRID_PACKAGE %in% installed.packages ( )) {
	install.packages (GRID_PACKAGE)
}
require (data.table)
require (ggplot2)
require (grid)

# Reads the emissions data.
# The data is read only if it's not previously loaded in memory.
print ("Reading emissions data...")
if (!exists (EMISSIONS_DATA_VARIABLE)) {
	emissions_data <- readRDS (DATA_FILE)
}

# Reads the sources data.
# The data is read only if it's not previously loaded in memory.
print ("Reading sources data...")
if (!exists (SOURCES_DATA_VARIABLE)) {
	emissions_sources <- readRDS (SOURCES_FILE)
}

# Creates a PNG file graphic device to hold the plot.
png (filename = PNG_PLOT_FILE, width = PNG_WIDTH, height = PNG_HEIGHT)

# The data subsetting takes place in two steps:
#
# 1. We need to know which emission sources are related to motor vehicles.
# 2. We need to filter out the emissions according to the results of the first step.

# First step: filtering emission sources.
# According to the data, there is no exact answer to the question "which sources are motor vehicle related?".
# A discussion about the different approaches:
# https://class.coursera.org/exdata-013/forum/thread?thread_id=106
# We will consider a valid source these with "gasoline" or "diesel" in the `EI.Sector` column.
# However, we need to remove the "Bulk Gasoline Terminals" source.
print ("Filtering motor vehicle related sources...")
motor_vehicle_discriminants <- grepl (pattern = MOTOR_VEHICLE_SOURCES_PATTERN, emissions_sources$EI.Sector, ignore.case = TRUE)

motor_vehicle_sources <- emissions_sources [motor_vehicle_discriminants, ]
motor_vehicle_sources <- motor_vehicle_sources [motor_vehicle_sources$EI.Sector != EXTRANEOUS_SOURCE, ]

# Second step: filtering emission data.
print ("Filtering motor vehicle emissions...")
motor_vehicle_emissions <- emissions_data [emissions_data$SCC %in% motor_vehicle_sources$SCC, ]

# We need to summarize the data, aggregating the emissions by year.
# We also subset the data, focusing in Baltimore and Los Angeles.
print ("Creating aggregated data...")
motor_vehicle_emissions <- subset (motor_vehicle_emissions, fips == BALTIMORE_CODE | fips == LOS_ANGELES_CODE)
motor_vehicle_emissions$city [motor_vehicle_emissions$fips == BALTIMORE_CODE] <- BALTIMORE_NAME
motor_vehicle_emissions$city [motor_vehicle_emissions$fips == LOS_ANGELES_CODE] <- LOS_ANGELES_NAME

emissions_per_year <- data.table (motor_vehicle_emissions) [, list (emissions = sum (Emissions) / SCALING_FACTOR), by = list (year, city)]

# We need to calculate the regression lines for each subset of data to get the slope.
# The slopes are considered in absolute terms, just to show the greater change.
print ("Calculating the slopes of the regression lines...")
fit_baltimore <- lm (year ~ emissions, data = emissions_per_year, city == BALTIMORE_NAME)
fit_los_angeles <- lm (year ~ emissions, data = emissions_per_year, city == LOS_ANGELES_NAME)

slope_baltimore <- abs (round (fit_baltimore$coefficients [[2]], digits = 2))
slope_los_angeles <- abs (round (fit_los_angeles$coefficients [[2]], digits = 2))

# This page helped with the plot:
# http://www.r-bloggers.com/using-r-barplot-with-ggplot2/
# Note that we need to transform the year to a factor variable.
# Otherwise, the X axis will get weird decimals...
print ("Creating the plot...")
p <- qplot (
	x = factor (year),
	y = emissions,
	fill = city,
	data = emissions_per_year,
	stat = STAT_TYPE,
	position = POSITION_ADJUSTMENT
)

# We need to define the aesthetics for the plot.
#
# * Geometry: points.
# * Facets: grid (two rows, one column), using the city name as discriminant.
# * Labels: iven by the corresponding constants.
# * Legend: not shown.
# * Color: "Set1" palette for the regression zones.
# * Regression line: based on the data for each city.
# * Margin: gives room enough to plot the main title.
plot_geometry <- geom_point ( )
plot_labels <- labs (x = PLOT_X_LABEL, y = PLOT_Y_LABEL, title = PLOT_TITLE)
plot_grid <- facet_grid (city ~ ., scales = "free_y")
plot_no_legend <- theme (legend.position = "none")
plot_colors <- scale_fill_brewer (palette = PLOT_COLOR_PALETTE)
plot_regression_line <- geom_smooth (method = "lm")
plot_regression_adjustment <- aes (group = 1)
plot_margins <- theme (
	plot.margin = unit (
		c (
			PLOT_MARGIN_TOP,
			PLOT_MARGIN_RIGHT,
			PLOT_MARGIN_BOTTOM,
			PLOT_MARGIN_LEFT
		),
		PLOT_MARGIN_UNITS
	)
)

# This fragment creates the text with the slopes.
xpos <- c (2, 2)
ypos <- c (1, 1)
lab <- c (
	paste (SLOPE_TEXT, slope_baltimore),
	paste (SLOPE_TEXT, slope_los_angeles)
)
city <- c (BALTIMORE_NAME, LOS_ANGELES_NAME)
label_data <- data.frame (xpos, ypos, lab, city)
plot_regression_text <- geom_text (
	data = label_data,
	aes (
		x = xpos,
		y = ypos,
		label = lab
	),
	size = 4
)

# Finally, this is the composition of all the items in the graph.
q <- (
	p +
	plot_geometry +
	plot_labels +
	plot_grid +
	plot_no_legend +
	plot_colors +
	plot_regression_line +
	plot_regression_adjustment +
	plot_regression_text +
	plot_margins
)
print (q)

# Closes the PNG file graphics device.
dev.off ( )