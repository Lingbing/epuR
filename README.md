# epuR: Economic Policy Uncertainty Data Analysis Made Easy in R

This package provides tools to retrieve Economic Policy Uncertainty (EPU) data from the EPU official website (https://www.policyuncertainty.com/index.html). The website is hosting a collection of EPU related indices, including EPU indices in dozens of countries, categorical EPU, and some other indices like Trade Policy Index, Financial Stress Indicator, Geopolitical Risk index, etc. Hand collecting from the website can be cumbersome, and putting them into the right format for further analysis can be frustrating. 

`epuR` provides functions to collect indicidual index in real time from the EPU and related websites while processing the dates and formats. The output is an `xts` data object which makes further manipulation, plotting, and analysis easier.

The following indices are supported:

1. Economic Policy Uncertainty (EPU) using `get_epu()`.
2. Trade Policy Uncertainty (TPU) using `get_tpu()`.
3. US Equity Market Volatility (EMV)
4. World Uncertainty Index (WUI)
5. Financial Stress Indicator (FSI)
6. Geopolitical Risk Index (GRI)
7.Migration Fear Index (MFI) and EPU data for France, Germany, the UK, and the US.

Monetary Policy Uncertainty (MPU) from https://sites.google.com/site/lucasfhusted/data

World Uncertainty Index (WUI) from https://worlduncertaintyindex.com/data/

ERUQ (Economic Uncertainty Relation Queries) data are hosted on Dropbox
