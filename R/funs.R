get_epu <- function(region = "all") {
  library(tidyverse)
  library(openxlsx)
  url <- "https://www.policyuncertainty.com/media/All_Country_Data.xlsx"
  data <- read.xlsx(url, rows = 1:423)
  data_date <- as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")

  data <- data %>%
    select(-c(Year, Month))
  data_xts <- xts(data, order.by = data_date)
  ind <- colnames(data)
  if(region == "all") {
    region_data <- data_xts
  }
  else if (!(region %in% ind)) {
    cat("region name not in the data, try one of ", "\n", ind, "\n")
  }
  else {
    region_data <- na.omit(data_xts[, region])
  }
  return(region_data)
}
