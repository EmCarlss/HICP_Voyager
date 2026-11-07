# Contributions since a reference period

## Change contributions

For a monthly reference, the Index tab implements the direct arbitrary-period decomposition in Schwind and Weinand (2026), equations (5)–(7). For a base month in year `b` and a target month in year `y`, each direct child's contribution consists of:

- a price-updated base-year term from the selected month through December of year `b`;
- one term for every complete intervening calendar year, using that year's normalized parent-relative weight and its December-to-December price relative; and
- a current-year term from December `y-1` through the target month.

For targets in the base year, only the price-updated intra-year term is used. Contributions are percentage-point contributions to the selected parent aggregate's price change. Annual child weights are divided by the published parent weight. They are not rescaled inside the Schwind–Weinand contribution calculation.

This direct calculation is separate from the Ribe contributions on the M/M-1 and M/M-12 tabs. It exposes annual weights and December chain links explicitly and retains any residual against the published parent index.

## Full-year reference periods

A full-year reference uses all twelve months; an incomplete year is not offered. Voyager first calculates the monthly decomposition from a common complete anchor month. Let:

- `R[t]` be the published parent index rebased to that anchor, with `R[anchor] = 100`;
- `C[i,t]` be component `i`'s contribution from the same anchor;
- `A` be the mean of `R[t]` over the twelve months in reference year `Y`; and
- `mean_Y(C[i])` be component `i`'s mean contribution in that year.

Voyager then applies the derived change-of-reference transformation

```text
R_year[t]   = (100 / A) * R[t]
C_year[i,t] = (100 / A) * (C[i,t] - mean_Y(C[i]))
```

Consequently, `mean_Y(R_year) = 100` and `mean_Y(C_year[i]) = 0`. If the anchor decomposition is additive, then

```text
sum_i C_year[i,t] = R_year[t] - 100.
```

The same transformation also scales any observed aggregation residual instead of hiding it. Because the common anchor precedes the reference year, the transformed series can include targets both before and after that year where the necessary data exist.

This change-of-reference transformation is a Voyager derivation used to support a monthly display relative to an annual average. It should not be confused with a formula quoted directly from Schwind and Weinand.

## Allocated index levels

Schwind–Weinand contributions decompose a price change; they do not allocate an index level. The combined chart therefore uses a separate presentation measure. For component `i`,

```text
L[i,t] = 100 * s[i] + C[i,t],
```

where `C[i,t]` is the contribution for the selected reference-period type and `s[i]` is a normalized level share. The shares are based on price-updated component weights:

- for a monthly reference, the selected month's price-updated weights are normalized to sum to one;
- for a full-year reference, the twelve monthly price-updated weights are averaged component by component and then normalized to sum to one.

This normalization is used only to distribute the baseline level 100. It does not modify published weights or change contributions. With complete data,

```text
sum_i L[i,t] = 100 + sum_i C[i,t].
```

The stacked components therefore form the reconstructed index level, while the dark line is the published rebased parent index. Their difference is the aggregation residual. Residuals are never allocated back to components.

The component hover reports `C[i,t]`—the contribution to price change in percentage points—not the allocated level `L[i,t]` used as bar height.

## Negative levels and incomplete observations

Plotly's relative stacking puts positive and negative bars on opposite sides of zero, so its visible top is not the algebraic sum when an allocated component level is negative. Voyager checks the allocated levels before drawing the chart. If any complete observation has a negative allocated level, it shows the components as non-stacked lines and displays a warning rather than claiming that a misleading stack equals the index.

If a required component index, annual weight, or December link is unavailable, that contribution is `NA`. The complete contribution sum, residual, and all plotted component levels for that month are also omitted. Available component values remain in the internal result and CSV as a labelled partial sum, but they are not drawn as an apparently complete reconstruction.

## Data retrieval and plot updates

`Retrieve data` downloads the monthly HICP (`prc_hicp_minr`, unit `I25`) and annual item weights (`prc_hicp_iw`) for the selected country, parent, and complete direct-child partition. It uses the existing Eurostat cache and populates the valid monthly and full-year reference choices.

`Update plot` applies the selected reference period to those already retrieved data. Changing the reference type or period does not issue another Eurostat request. If the controls no longer match the displayed result, the previous chart remains visible with a status message until the user retrieves or updates as required.

## Diagnostics and export

The CSV retains both contribution and presentation fields, including:

- the selected reference type and period plus the common anchor;
- the requested-period contribution and the original anchor contribution;
- normalized level share, baseline allocation, and allocated component level;
- published and reconstructed rebased indices;
- complete and partial contribution sums;
- residual, missing-component, completeness, and negative-level flags.

Hover labels intentionally omit these diagnostics. Component hover contains only month, ECOICOP label, and signed contribution; published-index hover contains only month and the index value.

## Hierarchy requirements

Components must be exactly the direct children recorded for the selected parent in `coicop_set_hierarchy`. Duplicate, incomplete, empty, or prefix-overlapping partitions are rejected before retrieval. The mode remains limited to ordinary HICP and ECOICOP so HICP-CT and special-aggregate behavior elsewhere in Voyager is unchanged.

## Reference

Patrick Schwind and Sebastian Weinand (2026), “Contributions to euro area inflation over arbitrary time periods”, 19th Meeting of the Ottawa Group on Price Indices. The paper develops contributions to price changes for annually chained Laspeyres-type indices over arbitrary periods; Voyager's level allocation and monthly-to-full-year display transformation are identified above as derived presentation choices.

https://stats.unece.org/ottawagroup/download/Meeting_19_session6_paper_Schwind_Weinand_2026_Contributions_to_euro_area_inflation_over_arbitrary_time_periods.pdf
