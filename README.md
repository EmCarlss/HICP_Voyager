# HICP Voyager – Inflation Dashboard for Europe

**HICP Voyager** is an interactive R Shiny dashboard for exploring inflation data across European countries, based on the Harmonised Index of Consumer Prices (HICP).

The app allows users to analyse HICP developments by country, COICOP aggregate, special aggregate and time period. Data are retrieved from Eurostat using the R `eurostat` package.

## Features

- Analyse chained HICP indices across European countries.
- Compare monthly rates of change (M/M-1), annual rates of change (M/M-12), seasonal patterns and HICP weights.
- Explore unchained indices relative to December of the previous year (M/December Y-1).
- Compare standard HICP with HICP-CT, i.e. HICP at constant tax rates.
- Rebase indices to a selected index reference period.
- View contributions to monthly and annual rates of change for selected aggregates.
- Quickly select predefined country groups, such as the euro area, Nordic, Western, Eastern, Benelux and Balkan countries.
- Analyse both regular COICOP aggregates and selected special aggregates.

## Online version

The app is available here:

https://emcarlss.shinyapps.io/HICP_Voyager/

## Data source

Data are retrieved from Eurostat using the R `eurostat` package.

## Screenshots

### Chained indices

<img width="1432" height="786" alt="Chained HICP indices across selected countries" src="https://github.com/user-attachments/assets/73886a0b-14aa-4089-aedf-afc4d1231903" />

### Monthly rates of change, including contributions from sub-aggregates

<img width="2148" height="880" alt="Monthly HICP rates of change including contributions from sub-aggregates" src="https://github.com/user-attachments/assets/bb62db7d-8904-4ca9-93b8-a3dd22b7ddac" />

### Annual rates of change by country, including contributions from sub-aggregates

<img width="2212" height="1718" alt="Annual HICP rates of change by country including contributions from sub-aggregates" src="https://github.com/user-attachments/assets/9685d857-0ce8-49ba-b44f-63b67eb6d285" />

### Index seasonality

<img width="2212" height="1718" alt="HICP index seasonality relative to December of the previous year" src="https://github.com/user-attachments/assets/069c660c-6b5b-4c14-9bfc-65da32638c79" />

### HICP weights by year

<img width="2224" height="924" alt="HICP item weights by year" src="https://github.com/user-attachments/assets/b6ccb7f1-4f56-4d49-9023-3032e4baec3f" />

### HICP weights by country

<img width="2224" height="924" alt="HICP item weights by country" src="https://github.com/user-attachments/assets/3766b3d8-e0d3-4ce7-91af-9c2005924c22" />
