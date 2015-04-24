# plot3.R
# PM2.5 emissions evolution per type in Baltimore, Maryland, over the period 1999-2008.
# Course: Exploratory Data Analysis.
# Data Science Specialization.
# Second course project.
# Author: Pablo César Pérez González (pcesarperez@gmail.com)


# Constants.
#
# * `DATA_TABLE_PACKAGE`: Name of the `data.table` package, used in data transformation.
# * `GGPLOT2_PACKAGE`: Name of the `ggplot2` package, used to plot the data.
# * `EMISSIONS_DATA_VARIABLE`: Name of the variable wich holds the emissions data.
# * `DATA_FILE`: PM2.5 emissions data filename.
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
# * `BALTIMORE_CODE`: Federal Information Processing Standard code for Baltimore, Maryland.
DATA_TABLE_PACKAGE <- "data.table"
GGPLOT2_PACKAGE <- "ggplot2"
EMISSIONS_DATA_VARIABLE <- "emissions_data"
DATA_FILE <- "summarySCC_PM25.rds"
PNG_PLOT_FILE <- "plot3.png"
PNG_WIDTH <- 512
PNG_HEIGHT <- 512
PLOT_TITLE <- expression ("PM"[2.5] ~ "emissions per year and type (Baltimore, Maryland)")
PLOT_TYPE <- "bar"
STAT_TYPE <- "identity"
POSITION_ADJUSTMENT <- "dodge"
PLOT_X_LABEL <- "Year"
PLOT_Y_LABEL <- "Emissions (tons)"
PLOT_COLOR_PALETTE <- "Greens"
BALTIMORE_CODE <- 24510


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
require (data.table)
require (ggplot2)

# Reads the emissions data.
# The data is read only if it's not previously loaded in memory.
print ("Reading emissions data...")
if (!exists (EMISSIONS_DATA_VARIABLE)) {
	emissions_data <- readRDS (DATA_FILE)
}

# Creates a PNG file graphic device to hold the plot.
png (filename = PNG_PLOT_FILE, width = PNG_WIDTH, height = PNG_HEIGHT)

# We need to summarize the data, aggregating the emissions by year and type.
# We need also to create a subset of the data, focusing in Baltimore, Maryland.
print ("Creating aggregated data...")
emissions_in_baltimore = subset (emissions_data, fips == BALTIMORE_CODE)
emissions_per_year_and_type_in_baltimore <- data.table (emissions_in_baltimore) [, list (emissions = sum (Emissions)), by = list (year, type)]

# This page helped with the plot:
# http://www.r-bloggers.com/using-r-barplot-with-ggplot2/
# Note that we need to transform the year to a factor variable.
# Otherwise, the X axis will get weird decimals...
print ("Creating the plot...")
p <- qplot (
	x = factor (year),
	y = emissions,
	fill = type,
	data = emissions_per_year_and_type_in_baltimore,
	geom = PLOT_TYPE,
	stat = STAT_TYPE,
	position = POSITION_ADJUSTMENT
)

# We need to define the aesthetics for the plot (labels, facets, colors, and legend).
#
# * The labels are defined in the constants section.
# * The facets arrange the plot using the `type` column as discriminator.
# * The colors define a palette of greens.
# * Finally, we get rid of the legend, as the facets have the names of the types on the top.
plot_labels <- labs (x = PLOT_X_LABEL, y = PLOT_Y_LABEL, title = PLOT_TITLE)
plot_facet <- facet_wrap (~ type)
plot_colors <- scale_fill_brewer (palette = PLOT_COLOR_PALETTE)
plot_no_legend <- theme (legend.position = "none")

print (p + plot_labels + plot_facet + plot_colors + plot_no_legend)

# Closes the PNG file graphics device.
dev.off ( )