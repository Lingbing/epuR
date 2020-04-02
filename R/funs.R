#' Get (Economic Policy Uncertainty) EPU indices
#'
#' Takes in the region name
#' @param region a character indicating the region of the EPU you want. The default
#' is "all" regions. The region names has to be one of the options in the EPU country list
#' @return return an xts data object containing the EPU for the chosen region
#' @seealso xts
#' @import xts zoo magrittr
#' @importFrom openxlsx read.xlsx
#' @export
#' @references \url{https://www.policyuncertainty.com/}
#' @examples
#' # get all country data by default
#' data <- get_epu()
#' # it is a xts object so it can be plotted directly
#' plot(data)
#' # use dygraphs for interactive ts plot
#' library(dygraphs)
#' dygraph(data)
#' # get country-wise data using specific region name
#' china_epu <- get_epu("China")
#' library(dygraphs)
#' dygraph(china_epu)
get_epu <- function(region = "all") {
  url <- "https://www.policyuncertainty.com/media/All_Country_Data.xlsx"
  data <- read.xlsx(url, rows = 1:423)
  data_date <- as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")

  data <- subset(data, select = -c(Year, Month))
  data_xts <- xts(data, order.by = data_date)
  ind <- colnames(data)
  if(region == "all") {
    region_data <- data_xts
  }
  else if (!(region %in% ind)) {
    cat("Region name not in the data, try one of ", "\n", ind, "\n")
  }
  else {
    region_data <- na.omit(data_xts[, region])
  }
  return(region_data)
}

#' Get Trade Policy Uncertainty (TPU) indices
#'
#' Takes in the region name
#' @param region a character indicating the region of the TPU you want. The default
#' is "China" regions. Can also be "Japan" and "US".
#' @return return an xts data object containing the TPU for the chosen region
#' @seealso xts
#' @import xts zoo magrittr
#' @importFrom openxlsx read.xlsx
#' @export
#' @references \url{https://www.policyuncertainty.com/}
#' @examples
#' china_tpu <- get_tpu()
#' plot(china_tpu)
#' # get Japan TPU
#' jap_tpu <- get_tpu("Japan")
#' library(dygraphs)
#' dygraph(jap_tpu)
get_tpu <- function(region = "China") {
  if (region == "China") {
    url = "https://www.policyuncertainty.com/media/China_Mainland_Paper_EPU.xlsx"
    data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 4, cols = 1:3)
    data_date <- as.yearmon(paste(data$year, data$month, sep = "-"), format = "%Y-%m")
    data <- subset(data, select = TPU)
    data_xts <- xts(data, order.by = data_date)
  } else if (region == "Japan") {
    url = "https://www.policyuncertainty.com/media/Japan_Policy_Uncertainty_Data.xlsx"
    data <- read.xlsx(url, skipEmptyRows = TRUE, cols = 1:7)
    data_date <- as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")
    data <- subset(data, select = -c(Year, Month))
    data_xts <- xts(data, order.by = data_date)
    names(data_xts) <- c("EPU", "FPU", "MPU", "TPU", "ERPU")
  } else if (region == "US") {
    url <- "https://www.policyuncertainty.com/media/Trade_Uncertainty_Data.xlsx"
    data <- read.xlsx(url, rows = 1:417, cols = 1:3)
    data_date <- as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")
    data <- subset(data, select = -c(Year, Month))
    data_xts <- xts(data, order.by = data_date)
  }
  return(data_xts)
}





# get_TPU2 <- function(freq = "monthly") {
#   # the original data set contains other information such as exchange rate uncertainty
#   # and tarrifvol, etc. We only pick the TPU column
#   if (freq == "monthly") {
#     data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 3, detectDates = TRUE)
#     data_date <- as.yearmon(data$DATE, format = "%Y-%m")
#     data <- data %>%
#       select(TPU)
#     data_xts <- xts(data, order.by = data_date)
#   } else if (freq == "daily") {
#     data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 4)
#     data_date <- ymd(data$DAY)
#     data <- data %>%
#       select(TPUD)
#     data_xts <- xts(data, order.by = data_date)
#   } else if (freq == "quaterly") {
#     data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 5)
#     data_date <- as.yearqtr(data$DATEQ)
#     data <- data %>%
#       select(TPUQ)
#     data_xts <- xts(data, order.by = data_date)
#   }
#   return(data_xts)
# }

# get_GPR <- function() {
#   url = "https://www.matteoiacoviello.com/gpr_files/gpr_web_latest.xlsx"
#   data <- read.xlsx(url, sheet = 3)
#   data_date <- as.yearmon(data$Month, format = "%Y-%m")
#   data <- data %>%
#     select(-c(Year, Month))
#   data_xts <- xts(data, order.by = data_date)
# }




get_FSI <- function(freq = "monthly") {
  url = "https://www.policyuncertainty.com/media/Financial_Stress.xlsx"
  if (freq == "monthly") {
    data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 2, detectDates = TRUE)
    data_date <- as.yearmon(paste(data$year, data$month, sep = "-"), format = "%Y-%m")
    data <- data %>%
      select(-c(year, month, date))
  } else if (freq == "quarterly") {
    data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 1, detectDates = TRUE)
    data_date <- as.yearqtr(data$date)
    data <- data %>%
      select(-c(year, quarter, date))
  }
    data_xts <- xts(data, order.by = data_date)
}




