utils::globalVariables(c("Date", "Month", "TPU", "Year", "month", "quarter", "year"))

#' Get Economic Policy Uncertainty (EPU) data
#'
#' @description
#' EPU index from the official website while processing the dates and output formats.
#' @param region a character indicating the region of the EPU. The default
#' is "all" regions. The region names has to be one of the options in the EPU country list
#' @return an xts data object containing the EPU for the chosen region.
#' @seealso \code{\link{xts}}
#' @importFrom xts xts
#' @importFrom openxlsx read.xlsx
#' @importFrom zoo as.yearmon
#' @importFrom stats na.omit
#' @export
#' @references \url{https://www.policyuncertainty.com/}
#' @examples \dontrun{
#' data <- get_EPU()
#' # it is an xts object so it can be plotted directly
#' plot(data)
#' # use dygraphs for interactive ts plot
#' library(dygraphs)
#' dygraph(data)
#' # get country-wise data using specific region name
#' china_epu <- get_EPU("China")
#' dygraph(china_epu)
#' }
get_EPU <- function (region = "all") {
  url <- "https://www.policyuncertainty.com/media/All_Country_Data.xlsx"
  data <- openxlsx::read.xlsx(url, rows = 1:423)
  data_date <- zoo::as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")

  data <- subset(data, select = -c(Year, Month))
  data_xts <- xts::xts(data, order.by = data_date)
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
#' @param region a character indicating the region of the TPU, default is "China", can be "Japan" or "US".
#' @return an xts data object containing the TPU for the chosen region
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://www.policyuncertainty.com/trade_uncertainty.html}
#' @examples \dontrun{
#' china_tpu <- get_TPU()
#' plot(china_tpu)
#' # get Japan TPU
#' jap_tpu <- get_TPU("Japan")
#' library(dygraphs)
#' dygraph(jap_tpu)
#' }
get_TPU <- function (region = "China") {
  if (region == "China") {
    url = "https://www.policyuncertainty.com/media/China_Mainland_Paper_EPU.xlsx"
    data <- openxlsx::read.xlsx(url, skipEmptyRows = TRUE, sheet = 4, cols = 1:3)
    data_date <- zoo::as.yearmon(paste(data$year, data$month, sep = "-"), format = "%Y-%m")
    data <- subset(data, select = TPU)
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (region == "Japan") {
    url = "https://www.policyuncertainty.com/media/Japan_Policy_Uncertainty_Data.xlsx"
    data <- openxlsx::read.xlsx(url, skipEmptyRows = TRUE, cols = 1:7)
    data_date <- zoo::as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")
    data <- subset(data, select = -c(Year, Month))
    data_xts <- xts::xts(data, order.by = data_date)
    names(data_xts) <- c("EPU", "FPU", "MPU", "TPU", "ERPU")
  } else if (region == "US") {
    url <- "https://www.policyuncertainty.com/media/Trade_Uncertainty_Data.xlsx"
    data <- openxlsx::read.xlsx(url, rows = 1:417, cols = 1:3)
    data_date <- zoo::as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")
    data <- subset(data, select = -c(Year, Month))
    data_xts <- xts::xts(data, order.by = data_date)
  }
  return(data_xts)
}


#' Get US Equity Market Volatility Index (EMV) data
#'
#' @param all logical, if TRUE return all EMV categories.
#' @return an xts data object
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://www.policyuncertainty.com/EMV_monthly.html}
#' @examples \dontrun{
#' emv_data <- get_EMV(all = FALSE)
#' plot(emv_data)
#' }
get_EMV <- function (all = T) {
  url = "https://www.policyuncertainty.com/media/EMV_Data.xlsx"
  data <- openxlsx::read.xlsx(url, rows = 1:423)
  data_date <- zoo::as.yearmon(paste(data$Year, data$Month, sep = "-"), format = "%Y-%m")
  data <- subset(data, select = -c(Year, Month))
  if(all) {
    data_xts <- xts::xts(data, order.by = data_date)
  } else {
    data_xts <- xts::xts(data$Overall.EMV.Tracker, order.by = data_date)
  }
  return(data_xts)
}

#' Get Financial Stress Indicator (FSI) data
#'
#' @param freq either "monthly" or "quarterly".
#' @return an xts data object.
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://www.policyuncertainty.com/financial_stress.html}
#' @examples \dontrun{
#' # for monthly FSI
#' fsi_mon <- get_FSI()
#' plot(fsi_mon)
#' fsi_quar <- get_FSI("quarterly")
#' plot(fsi_quar)
#' }
get_FSI <- function(freq = "monthly") {
  url = "https://www.policyuncertainty.com/media/Financial_Stress.xlsx"
  if (freq == "monthly") {
    data <- openxlsx::read.xlsx(url, skipEmptyRows = TRUE, sheet = 2, detectDates = TRUE)
    data_date <- zoo::as.yearmon(paste(data$year, data$month, sep = "-"), format = "%Y-%m")
    data <- subset(data, select = -c(year, month, date))
  } else if (freq == "quarterly") {
    data <- read.xlsx(url, skipEmptyRows = TRUE, sheet = 1, detectDates = TRUE)
    data_date <- zoo::as.yearqtr(data$date)
    data <- subset(data, select = -c(year, quarter, date))
  }
    data_xts <- xts::xts(data, order.by = data_date)
}

#' Get World Uncertainty Index (WUI) data
#'
#' @param type sheet option from the official excel file.
#' @return an xts data object
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://worlduncertaintyindex.com/data/}
#' @examples \dontrun{
#' wui_ave <- get_WUI("F1")
#' plot(wui_ave)
#' }
get_WUI <- function(type = "F1") {
  url = "https://worlduncertaintyindex.com/wp-content/uploads/2020/02/WUI_Data.xlsx"
  if (type == "F1") {
    data <- openxlsx::read.xlsx(url, startRow = 3, sheet = 2, cols = c(1, 3), detectDates = TRUE)
    data_date <- zoo::as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (type == "F2") {
    data <- openxlsx::read.xlsx(url, startRow = 3, sheet = 3, cols = 1:2, detectDates = TRUE)
    data_date <- zoo::as.yearqtr(data$year)
    data <- stats::na.omit(subset(data, select = -year))
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (type == "T1") {
    data <- openxlsx::read.xlsx(url, sheet = 4, detectDates = TRUE)
    data_date <- zoo::as.yearqtr(data$Year)
    data <- subset(data, select = -Year)
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (type == "T2") {
    data <- openxlsx::read.xlsx(url, sheet = 5, detectDates = TRUE)
    data_date <- zoo::as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (type == "T3") {
    data <- openxlsx::read.xlsx(url, sheet = 6, detectDates = TRUE)
    data_date <- zoo::as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (type == "T4") {
    data <- openxlsx::read.xlsx(url, sheet = 7, detectDates = TRUE)
    data_date <- zoo::as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (type == "T5") {
    data <- openxlsx::read.xlsx(url, sheet = 8, detectDates = TRUE)
    data_date <- zoo::as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (type == "T6") {
    data <- openxlsx::read.xlsx(url, sheet = 9, detectDates = TRUE)
    data_date <- zoo::as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts::xts(data, order.by = data_date)
  } else if(type == "T8") {
    data <- openxlsx::read.xlsx(url, sheet = 11, detectDates = TRUE)
    data_date <- zoo::as.yearqtr(data$year)
    data <- subset(data, select = -year)
    data_xts <- xts::xts(data, order.by = data_date)
  }
  return(data_xts)
}

#' Get Immigration Related Index (IRI) data
#'
#' @param region choose from UK, USA, Germany, France for Migrant related EPU and IRI for the specified region. Default returns all regions data.
#' @return an xts data object
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://www.policyuncertainty.com/immigration_fear.html}
#' @examples \dontrun{
#' usa_iri <- get_IRI("USA")
#' plot(usa_iri)
#' }
get_IRI <- function(region="all") {
  url = "https://www.policyuncertainty.com/media/Migration_Fear_EPU_Data.xlsx"
  data <- openxlsx::read.xlsx(url)
  data_date <- zoo::as.yearqtr(paste(data$year, data$quarter, sep = "-"))
  data <- subset(data, select = -c(year, quarter))
  data_xts <- xts::xts(data, order.by = data_date)
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
#' @param type a numeric indicating the type. 1 for quarterly GRI, 2 for GPRH, 3 for GPR of countries, and 4 for GPR words.
#' @return an xts data object
#' @seealso \code{\link{xts}}
#' @export
#' @references \url{https://www.matteoiacoviello.com/gpr.htm}
#' @examples \dontrun{
#' gpr <- get_GPR(1)
#' plot(gpr$GPR)
#' }
get_GPR <- function(type = 1) {
  url = "https://www.matteoiacoviello.com/gpr_files/gpr_web_latest.xlsx"
  if (type == 1) {
    data <- openxlsx::read.xlsx(url, sheet = 1, detectDates = T, rows = 1:424)
    data_date <- zoo::as.yearmon(as.Date(data$Date,origin = "1899-12-30"), format = "%y%b")
    data <- subset(data, select = -Date)
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (type == 2) {
    data <- openxlsx::read.xlsx(url, sheet = 2)
    data_date <- zoo::as.yearmon(paste(floor(data$Year),
                                  match(data$Month, month.name),
                                  sep = "-"), format = "%Y-%m")
    data <- subset(data, select = -c(Year, Month))
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (type == 3) {
    data <- read.xlsx(url, sheet = 3, detectDates = T)
    data_date <- zoo::as.yearmon(as.Date(data$Date,origin = "1899-12-30"), format = "%y%b")
    data <- subset(data, select = -Date)
    data_xts <- xts::xts(data, order.by = data_date)
  } else if (type == 4) {
    data <- openxlsx::read.xlsx(url, sheet = 4, detectDates = T)
    data_date <- zoo::as.yearmon(as.Date(data$Date,origin = "1899-12-30"), format = "%y%b")
    data <- subset(data, select = -Date)
    data_xts <- xts::xts(data, order.by = data_date)
  }
}



#' Get Oxford-Man Institute (OMI) data
#'
#' @description
#' Realized volatility data from the Oxford-Man Institute of Quantitative Finance website
#' @param index, a character string of the index name needed.
#' @return an xts data object
#' @seealso \code{\link{xts}}
#' @importFrom data.table fread
#' @importFrom lubridate ymd
#' @importFrom xts xts
#' @importFrom stringr str_sub
#' @export
#' @references \url{https://www.policyuncertainty.com/}
#' @examples \dontrun{
#' ## it take time to download data from OMI.
#' ## Its size is about 15Mb in zip
#' aex_data <- get_OMI("AEX")
#' str(aex_data)
#' }
get_OMI <- function(index = "AEX") {
  url <- "https://realized.oxford-man.ox.ac.uk/images/oxfordmanrealizedvolatilityindices.zip"
  temp = tempfile()
  download.file(url, temp)
  data <- data.table::fread(unzip(temp, file = "oxfordmanrealizedvolatilityindices.csv"))
  unlink(temp)
  data_date <- lubridate::ymd(substr(data$V1, 1, 10))
  data <- subset(data, select = -V1)
  data$date <- data_date
  index.list <- stringr::str_sub(unique(data$Symbol), 2)
  if (!(index %in% index.list)) {
    cat("Index has to be in the list \n", index.list, "\n")
    stop("Try a new index name")
  } else {
    index_data <- subset(data, Symbol == paste0(".", index))[, -1]
    index_date <- subset(data, Symbol == paste0(".", index), select = date)
    data_xts <- xts::xts(index_data, order.by = index_date$date)
  }
  return(data_xts)
}
