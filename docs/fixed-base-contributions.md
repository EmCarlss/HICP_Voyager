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

The same transformation also scales any observed aggregation residual instead of hiding it. Before offering a full year, Voyager verifies the annual weights and December chain links from each possible anchor through that year. It starts at the earliest anchor in the latest uninterrupted, computable chain. The transformed series can therefore include targets before the reference year when the chain supports them; history before a broken annual link is omitted and reported in the contribution-status message and CSV. This avoids calculating a long history that cannot reach the selected reference year.

This change-of-reference transformation is a Voyager derivation used to support a monthly display relative to an annual average. It should not be confused with a formula quoted directly from Schwind and Weinand.

## Cumulative-change chart

The combined chart displays `C[i,t]` directly as cumulative percentage-point contributions to the selected parent's price change. The reference period's rebased parent index is 100, while the chart's reference level is zero because it displays the change from 100:

```text
PublishedChange[t] = PublishedIndex[t] - 100.
```

Positive component contributions are stacked upward from zero and negative contributions downward from zero. The dark line is the published parent's cumulative change, not the component sum. With complete data,

```text
PublishedChange[t] = sum_i C[i,t] + Residual[t].
```

Residuals are reported and never distributed across components. When both positive and negative contributions occur, the published line generally does not lie at the top of the positive stack: for example, `+80` and `-10` sum algebraically to `+70`, so the positive stack reaches `+80`, the negative stack reaches `-10`, and the line lies at `+70` when the residual is zero.

## Incomplete observations

If a required component index, annual weight, or December link is unavailable, that contribution remains `NA`. The complete contribution sum and residual are also omitted. All component bars for that month are suppressed so the chart cannot suggest a complete decomposition, while the published parent change remains visible when its observation is available. Available component values remain in the internal result and CSV as a labelled partial sum.

## Data retrieval and plot updates

`Retrieve data` downloads the monthly HICP (`prc_hicp_minr`, unit `I25`) and annual item weights (`prc_hicp_iw`) for the selected country, parent, and complete direct-child partition. It uses the existing Eurostat cache and populates the valid monthly and full-year reference choices.

`Update plot` applies the selected reference period to those already retrieved data. Changing the reference type or period does not issue another Eurostat request. If the controls no longer match the displayed result, the previous chart remains visible with a status message until the user retrieves or updates as required.

## Diagnostics and export

The CSV retains the methodologically relevant contribution and diagnostic fields, including:

- the selected reference type and period plus the common anchor;
- the requested-period contribution and the original anchor contribution;
- published rebased index and published cumulative change;
- complete and partial contribution sums;
- residual, missing-component, completeness, and history-limit fields.

Hover labels intentionally omit these diagnostics. Component hover contains only month, ECOICOP label, and signed contribution. Published-line hover contains only month, the published rebased index, and its signed cumulative change.

## Hierarchy requirements

Components must be exactly the direct children recorded for the selected parent in `coicop_set_hierarchy`. Duplicate, incomplete, empty, or prefix-overlapping partitions are rejected before retrieval. The mode remains limited to ordinary HICP and ECOICOP so HICP-CT and special-aggregate behavior elsewhere in Voyager is unchanged.

## Reference

Patrick Schwind and Sebastian Weinand (2026), “Contributions to euro area inflation over arbitrary time periods”, 19th Meeting of the Ottawa Group on Price Indices. The paper develops contributions to price changes for annually chained Laspeyres-type indices over arbitrary periods; Voyager's monthly-to-full-year display transformation is identified above as a derived change-of-reference transformation.

https://stats.unece.org/ottawagroup/download/Meeting_19_session6_paper_Schwind_Weinand_2026_Contributions_to_euro_area_inflation_over_arbitrary_time_periods.pdf
