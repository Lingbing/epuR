# epuR: Economic Policy Uncertainty Data Analysis Made Easy

This package provides tools to retrieve Economic Policy Uncertainty (EPU) data from the EPU official website (https://www.policyuncertainty.com/index.html). The website is hosting a collection of EPU related indices, including EPU indices in dozens of countries, categorical EPU, and some other indices like Trade Policy Index, Financial Stress Indicator, Geopolitical Risk index, etc. Hand collecting from the website can be cumbersome, and putting them into the right format for further analysis can be frustrating. 

`epuR` provides individual functions to collect indicidual indices in real time from the website while processing the dates by its right manner. The otput is an `xts` data object which makes further manipulation, plotting, and analysis much easier.

Data can be retrieved completely or by country names. 
Other index from the website can also be collected, such as

1. The Trade Policy Index (TPI)
2. US equity market volatility (EMV)
3. World Uncertainty Index (WUI)
4. Financial Stress Indicator (FSI)
5. Geopolitical Risk Index (GRI)
6. Migration Fear Index (MFI) and EPU data for France, Germany, the UK, and the US.

Monetary Policy Uncertainty (MPU) from https://sites.google.com/site/lucasfhusted/data

World Uncertainty Index (WUI) from https://worlduncertaintyindex.com/data/

ERUQ (Economic Uncertainty Relation Queries) data are hosted on Dropbox
