required_packages <- c("googlesheets4")
installed_packages <- installed.packages()
missing_packages <- setdiff(required_packages, installed_packages[, "Package"])

if (length(missing_packages) > 0) {
    install.packages(missing_packages)
}

# Load required libraries
library(googlesheets4)
googlesheets4::gs4_deauth()

spreadsheet_url <- "https://docs.google.com/spreadsheets/d/10r3DV5vFsZOu7DPbqlNHDd-WW0NrRXuFe8J5mhg-bQ8/edit?gid=0#gid=0"
sheet_name <- "Blad1"

# Read the data from Google Sheets
data <- read_sheet(spreadsheet_url, sheet = sheet_name)

#plot from one sample
i <- "paramecium 240704"
average <- tapply(data$cell.density.cell.per.mL[data$sample == i], data$dilution[data$sample == i], mean)
data_subset <- data[data$sample == i, ]
data_summary <- aggregate(cells ~ dilution, data_subset, mean)
# Debug: Print the average to check if 1/16 dilution is included
print(average)

dilution <- unique(data$dilution)
dilution <- as.numeric(names(average))

quartz()

# Customize axes
fraction_labels <- c("1", "1/2", "1/4", "1/8", "1/16")  # Replace with your actual fractions
axis(1, at = dilution, labels = fraction_labels)
axis(2)


plot(dilution, average, type = "b", xlab = "Dilution", ylab = "Average Cell Count", main = paste("Average Cell Count for", i))

mod <- lm(average ~ dilution)

abline(mod, col='red')

cor.test(dilution, average)

plot(log2(dilution), average, type = "b", xlab = "Dilution", ylab = "Average Cell Count", main = paste("Average Cell Count for", i))

summary(data)