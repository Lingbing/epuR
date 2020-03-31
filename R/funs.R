get_epu <- function(region = "all") {
  library(tidyverse)
  library(openxlsx)
  library(lubridate)
  library(zoo)
  library(xts)
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
get_TPU <- function(region = "China") {
  if (region == "China") {
    url = "https://www.policyuncertainty.com/media/China_Mainland_Paper_EPU.xlsx"
    data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 4, cols = 1:3)
    data_date <- as.yearmon(paste(data$year, data$month, sep = "-"), format = "%Y-%m")
    data <- data %>%
      select(TPU)
    data_xts <- xts(data, order.by = data_date)
  } else if (region == "Japan") {
    url = "https://www.policyuncertainty.com/media/Japan_Policy_Uncertainty_Data.xlsx"
    data <- read.xlsx(url, skipEmptyRows = TRUE, cols = 1:7)
    data_date <- as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")
    data <- data %>%
      select(-c(Year, Month))
    data_xts <- xts(data, order.by = data_date)
    names(data_xts) <- c("EPU", "FPU", "MPU", "TPU", "ERPU")
  } else if (region == "US") {
    url <- "https://www.policyuncertainty.com/media/Trade_Uncertainty_Data.xlsx"
    data <- read.xlsx(url, rows = 1:417, cols = 1:3)
    data_date <- as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")
    data <- data %>%
      select(-c(Year, Month))
    data_xts <- xts(data, order.by = data_date)
  }
  return(data_xts)
}

get_TPU2 <- function(freq = "monthly") {
  # the original data set contains other informations such as exchange rate uncertainty
  # and tarrifvol, etc. We only pick the TPU column
  if (freq == "monthly") {
    data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 3, detectDates = TRUE)
    data_date <- as.yearmon(data$DATE, format = "%Y-%m")
    data <- data %>%
      select(TPU)
    data_xts <- xts(data, order.by = data_date)
  } else if (freq == "daily") {
    data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 4)
    data_date <- ymd(data$DAY)
    data <- data %>%
      select(TPUD)
    data_xts <- xts(data, order.by = data_date)
  } else if (freq == "quaterly") {
    data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 5)
    data_date <- as.yearqtr(data$DATEQ)
    data <- data %>%
      select(TPUQ)
    data_xts <- xts(data, order.by = data_date)
  }
  return(data_xts)
}



