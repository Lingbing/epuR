#' Get Economic Policy Uncertainty (EPU) data
#'
#' @description
#' `get_EPU()` gets EPU index from the official website while processing the
#' dates and output formats.
#'
#' Learn more in `vignette("epuR")`
#'
#' @details
#' For convenience, the `get_epu()` function get the full data consisting of all
#' regions in real time, so it may take a while, typically 20 seconds or so.
#' If region name is provided, then the regional EPU data is retrieved from the
#' full data xts object. It is suggested to get the full data by default and then
#' collects regional data using xts.
#'
#' @param region a character indicating the region of the EPU you want. The default
#' is "all" regions. The region names has to be one of the options in the EPU country list
#' @return return an xts data object containing the EPU for the chosen region
#' @seealso \code{\link{xts}}
#' @import xts magrittr openxlsx zoo
#' @export
#' @references \url{https://www.policyuncertainty.com/}
#' @examples
#' # get all country data by default
#' data <- get_EPU()
#' # it is an xts object so it can be plotted directly
#' plot(data)
#' # use dygraphs for interactive ts plot
#' library(dygraphs)
#' dygraph(data)
#' # get country-wise data using specific region name
#' china_epu <- get_EPU("China")
#' library(dygraphs)
#' dygraph(china_epu)
get_EPU <- function (region = "all") {
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

#' Get Trade Policy Uncertainty (TPU) data
#'
#' @param region a character indicating the region of the TPU you want. The default
#' is "China" regions. Can also be "Japan" and "US".
#' @return return an xts data object containing the TPU for the chosen region
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://www.policyuncertainty.com/trade_uncertainty.html}
#' @examples
#' china_tpu <- get_TPU()
#' plot(china_tpu)
#' # get Japan TPU
#' jap_tpu <- get_TPU("Japan")
#' library(dygraphs)
#' dygraph(jap_tpu)
get_TPU <- function (region = "China") {
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


#' Get US Equity Market Volatility Index (TPU) data
#'
#' @param all logical. if TRUE return all EMV categories.
#' @return return an xts data object
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://www.policyuncertainty.com/EMV_monthly.html}
#' @examples
#' emv_data <- get_EMV(all = FALSE)
#' dygraph(emv_data)
get_EMV <- function (all = T) {
  url = "https://www.policyuncertainty.com/media/EMV_Data.xlsx"
  data <- read.xlsx(url, rows = 1:423)
  data_date <- as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")
  data <- subset(data, select = -c(Year, Month))
  if(all) {
    data_xts <- xts(data, order.by = data_date)
  } else {
    data_xts <- xts(data$Overall.EMV.Tracker, order.by = data_date)
  }
  return(data_xts)
}






#' Get Financial Stress Indicator (FSI) data
#'
#' @details The FSI runs from 1889 to 2016 and is available at monthly and quarterly frequencies.
#' Index is constructed from the titles of articles published in five U.S.
#' newspapers: the Boston Globe, Chicago Tribune, Los Angeles Times, Wall Street Journal and Washington Post.
#'
#' @param freq either "monthly" or "quarterly"
#' @return return an xts data object
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://www.policyuncertainty.com/financial_stress.html}
#' @examples
#' # for monthly FSI
#' fsi_mon <- get_FSI()
#' dygraph(fsi_mon)
#' fsi_quar <- get_FSI("quarterly")
#' dygraph(fsi_quar)
get_FSI <- function(freq = "monthly") {
  url = "https://www.policyuncertainty.com/media/Financial_Stress.xlsx"
  if (freq == "monthly") {
    data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 2, detectDates = TRUE)
    data_date <- as.yearmon(paste(data$year, data$month, sep = "-"), format = "%Y-%m")
    data <- subset(data, select = -c(year, month, date))

  } else if (freq == "quarterly") {
    data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 1, detectDates = TRUE)
    data_date <- as.yearqtr(data$date)
    data <- subset(data, select = -c(year, quarter, date))
  }
    data_xts <- xts(data, order.by = data_date)
}



#' Get World Uncertainty Index (WUI) data
#'
#' @details The dataset includes the World Uncertainty Index (WUI) at the global level,
#' as well as by income, region, and country levels.
#'
#' @param type sheet option from the official dataset
#' F1: overall uncertainty across the globe.
#' F2: trade uncertainty across the globe.
#' T1: World Uncertainty Index (WUI) at the global level (simple average and GDP weighted average),
#' income level (advanced, emerging, and low-income economies), and regional level
#'  (Africa, Asia and the Pacific, Europe, Middle East and Central Asia,
#'  and Western Hemisphere).
#'  T2: World Uncertainty Index (WUI) for 143 countries
#'  T3:  total count of the word "uncertainty" (or its variant) in the EIU
#'  country reports for 143 countries and from the 1950s
#'  T4: count of the total number of words in the EIU reports for 143 countries
#'  and from the 1950s
#'  T5: count of the total number of pages in the EIU countries reports for
#'  143 countries from the 1950s
#'  T6：three-quarter weighted moving average of the World Uncertainty Index (WUI) for
#'  143 countries from the 1950s
#'  T8: aggregate World Trade Uncertainty (WTU) index as well as the index by country.
#' @return return an xts data object
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://worlduncertaintyindex.com/data/}
#' @examples
#' wui_ave <- get_WUI("F1")
#' dygraph(wui_ave)
get_WUI <- function(type = "F1") {
  url = "https://worlduncertaintyindex.com/wp-content/uploads/2020/02/WUI_Data.xlsx"
  if (type == "F1") {
    data <- read.xlsx(url, startRow = 3, sheet = 2, cols = c(1, 3), detectDates = TRUE)
    data_date <- as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts(data, order.by = data_date)
  } else if (type == "F2") {
    data <- read.xlsx(url, startRow = 3, sheet = 3, cols = 1:2, detectDates = TRUE)
    data_date <- as.yearqtr(data$year)
    data <- na.omit(subset(data, select = -year))
    data_xts <- xts(data, order.by = data_date)
  } else if (type == "T1") {
    data <- read.xlsx(url, sheet = 4, detectDates = TRUE)
    data_date <- as.yearqtr(data$Year)
    data <- subset(data, select = -Year)
    data_xts <- xts(data, order.by = data_date)
  } else if (type == "T2") {
    data <- read.xlsx(url, sheet = 5, detectDates = TRUE)
    data_date <- as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts(data, order.by = data_date)
  } else if (type == "T3") {
    data <- read.xlsx(url, sheet = 6, detectDates = TRUE)
    data_date <- as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts(data, order.by = data_date)
  } else if (type == "T4") {
    data <- read.xlsx(url, sheet = 7, detectDates = TRUE)
    data_date <- as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts(data, order.by = data_date)
  } else if (type == "T5") {
    data <- read.xlsx(url, sheet = 8, detectDates = TRUE)
    data_date <- as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts(data, order.by = data_date)
  } else if (type == "T6") {
    data <- read.xlsx(url, sheet = 9, detectDates = TRUE)
    data_date <- as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts(data, order.by = data_date)
  } else if(type == "T8") {
    data <- read.xlsx(url, sheet = 11, detectDates = TRUE)
    data_date <- as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts(data, order.by = data_date)
  }
  return(data_xts)
}

#' Get Immigration Related Index (IRI) data
#'
#' @details the intensity of migration-related fears in France, Germany,
#' the United Kingdom and the United States.
#' @param region choose from UK, USA, Germany, France for Migrant related EPU and
#' IRI for the specified region. Default is `all` that returns all regions data.
#' @return return an xts data object
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://www.policyuncertainty.com/immigration_fear.html}
#' @examples
#' usa_iri <- get_IRI("USA")
#' dygraph(usa_iri)
get_IRI <- function(region="all") {
  url = "https://www.policyuncertainty.com/media/Migration_Fear_EPU_Data.xlsx"
  data <- read.xlsx(url)
  data_date <- as.yearqtr(paste(data$year, data$quarter, sep = "-"))
  data <- subset(data, select = -c(year, quarter))
  data_xts <- xts(data, order.by = data_date)
  if(region == "UK") {
    final_data <- data_xts[, 1:2]
  } else if (region == "Germany") {
    final_data <- data_xts[, 3:4]
  } else if (region == "USA") {
    final_data <- data_xts[, 5:6]
  } else if (region == "France") {
    final_data <- data_xts[, 7:8]
  } else if (region == "all") {
    final_data <- data_xts
  }
  return(final_data)
}

#' Get Geopolitical Risk Index (GPR) data
#'
#' @details Dario Caldara and Matteo Iacoviello construct a monthly index
#' of Geopolitical Risk (GPR Index) counting the occurrence of words related to
#'  geopolitical tensions in leading international newspapers.
#'  The GPR index spikes around the Gulf War, after 9/11, during the 2003 Iraq invasion,
#'  during the 2014 Russia-Ukraine crisis, and after the Paris terrorist attacks.
#' @param type a numeric indicating the type. 1 for quarterly GRI, 2 for GPRH, 3 for GPR
#' of countries, and 4 for GPR words.
#' @return return an xts data object
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://www.matteoiacoviello.com/gpr.htm}
#' @examples
#' gpr <- get_GPR(1)
#' plot(gpr$GPR)
get_GPR <- function(type = 1) {
  url = "https://www.matteoiacoviello.com/gpr_files/gpr_web_latest.xlsx"
  # temp = tempfile(fileext = ".xlsx")
  # download.file(url, destfile=temp, mode='wb')
  # data <- readxl::read_xlsx(temp, sheet =1, col_types = c("date", rep("numeric", 12)))
  if (type == 1) {
    data <- read.xlsx(url, sheet = 1, detectDates = T, rows = 1:424)
    data_date <- as.yearmon(as.Date(data$Date,origin = "1899-12-30"), format = "%y%b")
    data <- subset(data, select = -Date)
    data_xts <- xts(data, order.by = data_date)
  } else if (type == 2) {
    data <- read.xlsx(url, sheet = 2)
    data_date <- as.yearmon(paste(floor(data$Year),
                                  match(data$Month, month.name),
                                  sep = "-"), format = "%Y-%m")
    data <- subset(data, select = -c(Year, Month))
    data_xts <- xts(data, order.by = data_date)
  } else if (type == 3) {
    data <- read.xlsx(url, sheet = 3, detectDates = T)
    data_date <- as.yearmon(as.Date(data$Date,origin = "1899-12-30"), format = "%y%b")
    data <- subset(data, select = -Date)
    data_xts <- xts(data, order.by = data_date)
  } else if (type == 4) {
    data <- read.xlsx(url, sheet = 4, detectDates = T)
    data_date <- as.yearmon(as.Date(data$Date,origin = "1899-12-30"), format = "%y%b")
    data <- subset(data, select = -Date)
    data_xts <- xts(data, order.by = data_date)
  }
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















