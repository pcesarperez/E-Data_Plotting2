# plot2.R
# PM2.5 emissions evolution in Baltimore, Maryland, over the period 1999-2008.
# Course: Exploratory Data Analysis.
# Data Science Specialization.
# Second course project.
# Author: Pablo César Pérez González (pcesarperez@gmail.com)


# Constants.
# 	`DATA_TABLE_PACKAGE`: Name of the `data.table` package, used in data transformation.
# 	`EMISSIONS_DATA_VARIABLE`: Name of the variable wich holds the emissions data.
# 	`DATA_FILE`: PM2.5 emissions data filename.
# 	`PNG_PLOT_FILE`: Filename of the PNG file which will hold the plot.
# 	`PNG_WIDTH`: Width of the PNG file.
# 	`PNG_HEIGHT`: Height of the PNG file.
# 	`PLOT_TITLE`: Title of the plot.
# 	`PLOT_TYPE`: Graph type (linear).
# 	`PLOT_X_LABEL`: Label of the X axis.
# 	`PLOT_Y_LABEL`: Label of the Y axis.
# 	`BALTIMORE_CODE`: Federal Information Processing Standard code for Baltimore, Maryland.
DATA_TABLE_PACKAGE <- "data.table"
EMISSIONS_DATA_VARIABLE <- "emissions_data"
DATA_FILE <- "summarySCC_PM25.rds"
PNG_PLOT_FILE <- "plot2.png"
PNG_WIDTH <- 512
PNG_HEIGHT <- 512
PLOT_TITLE <- expression ("PM"[2.5] ~ "emissions evolution (Baltimore, Maryland)")
PLOT_TYPE <- "l"
PLOT_X_LABEL <- "Year"
PLOT_Y_LABEL <- "Emissions (tons)"
BALTIMORE_CODE <- 24510


# The package `data.table` is needed to perform the data aggregation.
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

# Creates a PNG file graphic device to hold the plot.
png (filename = PNG_PLOT_FILE, width = PNG_WIDTH, height = PNG_HEIGHT)

# We need to summarize the data, aggregating the emissions by year.
# We need also to create a subset of the data, focusing in Baltimore, Maryland.
print ("Creating aggregated data...")
emissions_in_baltimore = subset (emissions_data, fips == BALTIMORE_CODE)
emissions_by_year_in_baltimore <- data.table (emissions_in_baltimore) [, list (emissions = sum (Emissions)), by = year]

print ("Creating the plot...")
with (emissions_by_year_in_baltimore, {
	plot (
		year,
		emissions,
		type = PLOT_TYPE,
		xlab = PLOT_X_LABEL,
		ylab = PLOT_Y_LABEL,
		main = PLOT_TITLE
	)
})

# Closes the PNG file graphic device.
dev.off ( )