# HICP Voyager - Inflation dashboard for Europe

A simple R Shiny dashboard for the Harmonised Index of Consumer Prices (HICP).

Features
  - Analyze chained indices, monthly rates (M/M-1),annual rates (M/M-12), seasonality (M/December Y-1, i.e. unchained indices) and weights across countries.
  - Compare HICP and HICP-CT
  - Quick selection of country groups (e.g. Nordic, Western, Eastern, Benelux, Balkan)
  - Rebase indices to preferred index reference period.
  - See contributions to monthly and annual rates of change for selected aggregates.

Online access:
https://emcarlss.shinyapps.io/HICP_Voyager/

Data retrieved from Eurostat using the R 'eurostat' package.
