# HICP Voyager - Inflation dashboard for the European HICP

A simple R Shiny dashboard for the Harmonised Index of Consumer Prices (HICP).

Features
  - Analyze chained indices, monthly rates (M/M-1),annual rates (M/M-12), seasonality (M/December Y-1, i.e. unchained indices) and weights across countries.
  - Rebase indices to preferred index reference period.
  - See contributions to monthly and annual rates of change for selected aggregates.

Live demo:
https://emcarlss.shinyapps.io/HICP_Voyager/

Data retrieved from Eurostat using the R 'eurostat' package.
