# plot5.R
# PM2.5 emissions of motor vehicle related sources in Baltimore, Maryland, over the period 1999-2008.
# Course: Exploratory Data Analysis.
# Data Science Specialization.
# Second course project.
# Author: Pablo César Pérez González (pcesarperez@gmail.com)


# Constants.
#
# * `DATA_TABLE_PACKAGE`: Name of the `data.table` package, used in data transformation.
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
# * `MOTOR_VEHICLE_SOURCES_PATTERN`: Regular expression pattern for motor vehicles related sources.
# * `BALTIMORE_CODE`: Federal Information Processing Standard code for Baltimore, Maryland.
# * `EXTRANEOUS_SOURCE`: Emission source included by mistake in the first filter of the sources.
DATA_TABLE_PACKAGE <- "data.table"
EMISSIONS_DATA_VARIABLE <- "emissions_data"
SOURCES_DATA_VARIABLE <- "emissions_sources"
DATA_FILE <- "summarySCC_PM25.rds"
SOURCES_FILE <- "Source_Classification_Code.rds"
PNG_PLOT_FILE <- "plot5.png"
PNG_WIDTH <- 512
PNG_HEIGHT <- 512
PLOT_TITLE <- expression ("PM"[2.5] ~ "emissions of motor vehicle related sources in Baltimore")
PLOT_X_LABEL <- "Year"
PLOT_Y_LABEL <- "Emissions (tons)"
MOTOR_VEHICLE_SOURCES_PATTERN <- "(gasoline|diesel)"
BALTIMORE_CODE <- 24510
EXTRANEOUS_SOURCE <- "Bulk Gasoline Terminals"


# The following packages need to be installed:
#
# * `data.table`
if (!DATA_TABLE_PACKAGE %in% installed.packages ( )) {
	install.packages (DATA_TABLE_PACKAGE)
}
require (data.table)

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
# We also subset the data, focusing in Baltimore.
print ("Creating aggregated data...")
motor_vehicle_emissions_in_baltimore <- subset (motor_vehicle_emissions, fips == BALTIMORE_CODE)
motor_vehicle_emissions_per_year_in_baltimore <- data.table (motor_vehicle_emissions_in_baltimore) [ , list (emissions = sum (Emissions)), by = year]

print ("Creating the plot...")
with (motor_vehicle_emissions_per_year_in_baltimore, {
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