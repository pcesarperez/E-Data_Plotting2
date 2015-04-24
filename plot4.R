# plot4.R
# PM2.5 emissions of coal combustion related sources, over the period 1999-2008.
# Course: Exploratory Data Analysis.
# Data Science Specialization.
# Second course project.
# Author: Pablo César Pérez González (pcesarperez@gmail.com)


# Constants.
#
# * `DATA_TABLE_PACKAGE`: Name of the `data.table` package, used in data transformation.
# * `GGPLOT2_PACKAGE`: Name of the `ggplot2` package, used to plot the data.
# * `EMISSIONS_DATA_VARIABLE`: Name of the variable wich holds the emissions data.
# * `SOURCES_DATA_VARIABLE`: Name of the variable which holds the different sources for the emissions.
# * `DATA_FILE`: PM2.5 emissions data filename.
# * `SOURCES_FILE`: Emissions sources data filename.
# * `PNG_PLOT_FILE`: Filename of the PNG file which will hold the plot.
# * `PNG_WIDTH`: Width of the PNG file.
# * `PNG_HEIGHT`: Height of the PNG file.
# * `PLOT_TITLE`: Title of the plot.
# * `PLOT_X_LABEL`: Label of the X axis.
# * `PLOT_Y_LABEL`: Label of the Y axis.
# * `SCALING_FACTOR`: Scaling factor to apply to the emissions.
# * `COMBUSTION_SOURCES_PATTERN`: Regular expression pattern for combustion related sources.
# * `COAL_SOURCES_PATTERN`: Regular expression for coal related sources.
DATA_TABLE_PACKAGE <- "data.table"
GGPLOT2_PACKAGE <- "ggplot2"
EMISSIONS_DATA_VARIABLE <- "emissions_data"
SOURCES_DATA_VARIABLE <- "emissions_sources"
DATA_FILE <- "summarySCC_PM25.rds"
SOURCES_FILE <- "Source_Classification_Code.rds"
PNG_PLOT_FILE <- "plot4.png"
PNG_WIDTH <- 512
PNG_HEIGHT <- 512
PLOT_TITLE <- expression ("PM"[2.5] ~ "emissions of coal combustion related sources")
PLOT_X_LABEL <- "Year"
PLOT_Y_LABEL <- "Emissions (thousands of tons)"
SCALING_FACTOR <- 1000
COMBUSTION_SOURCES_PATTERN <- "combustion"
COAL_SOURCES_PATTERN <- "coal"


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
# 1. We need to know which emission sources are related to coal combustion.
# 2. We need to filter out the emissions according to the results of the first step.

# First step: filtering emission sources.
# The combustion sources are tagged in the first level.
# The coal sources are tagged in the fourth level.
print ("Filtering coal combustion related sources...")
combustion_sources <- grepl (pattern = COMBUSTION_SOURCES_PATTERN, emission_sources$SCC.Level.One, ignore.case = TRUE)
coal_sources <- grepl (pattern = COAL_SOURCES_PATTERN, emission_sources$SCC.Level.Four, ignore.case = TRUE)

coal_combustion_sources <- emission_sources [(combustion_sources & coal_sources), ]

# Second step: filtering emission data.
print ("Filtering coal combustion emissions...")
coal_combustion_emissions <- emissions_data [emissions_data$SCC %in% coal_combustion_sources$SCC, ]

# We need to summarize the data, aggregating the emissions by year.
print ("Creating aggregated data...")
coal_combustion_emissions_per_year <- data.table (coal_combustion_emissions) [ , list (emissions = sum (Emissions) / SCALING_FACTOR), by = year]

print ("Creating the plot...")
with (coal_combustion_emissions_per_year, {
	barplot (
		emissions,
		names.arg = year,
		xlab = PLOT_X_LABEL,
		ylab = PLOT_Y_LABEL,
		main = PLOT_TITLE,
		ylim = c (0, signif (max (emissions) * 1.1, digits = 1))
	)
})

# Closes the PNG file graphics device.
dev.off ( )