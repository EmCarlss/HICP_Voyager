# Contributions since a fixed reference month

## Method

The Index tab's fixed-base mode implements the direct arbitrary-period decomposition in Schwind and Weinand (2026), equations (5)-(7). For a base month in year `b` and a target month in year `y`, each direct child's contribution is composed of:

- a price-updated base-year term from the selected base month through December of year `b`;
- one term for every complete intervening calendar year, using that year's normalized weight and its December-to-December price relative; and
- a current-year term from December `y-1` through the target month.

For targets in the same calendar year as the base month, only the price-updated intra-year term is used. Contributions are expressed in percentage points of the selected parent aggregate's percentage change. Annual child weights are divided by the published annual parent weight. They are not subsequently rescaled to make the child weights sum to one.

This direct implementation was chosen instead of accumulating Voyager's M/M-1 contributions. A scaled telescoping sum is exact only when every underlying monthly decomposition is itself exact for the same complete, non-overlapping component partition. The direct formula exposes the annual weights and December links explicitly, follows the paper, and allows the residual against the published parent index to remain visible.

## Data and diagnostics

The calculation requires the published monthly HICP (`prc_hicp_minr`, unit `I25`) for the parent and all direct ECOICOP children, plus annual item weights (`prc_hicp_iw`). It uses the existing in-session Eurostat request cache. Changing only the reference month recalculates from the already retrieved data and does not issue another Eurostat request.

For each target month the output contains:

- the parent index rebased to 100 in the selected month;
- every direct child's contribution;
- the published parent percentage change;
- the complete and partial contribution sums;
- the residual (published parent change minus complete contribution sum); and
- explicit missing-component and significant-residual flags.

If any required component index, annual weight, or December link is unavailable, that component contribution is `NA`. The complete sum and residual are then also `NA`; the available components are retained only as a labelled partial sum. No residual is allocated back to components.

## Hierarchy requirements

The selected components must be exactly the direct children recorded for the parent in `coicop_set_hierarchy`. Duplicate, incomplete, empty, or prefix-overlapping partitions are rejected before data retrieval. The first implementation is intentionally limited to ordinary HICP and ECOICOP so HICP-CT and special-aggregate behaviour remain unchanged.

## Full-year reference periods

The existing Compare indices mode continues to support rebasing to a full-year average. Fixed-base contribution mode currently supports monthly reference periods only. The paper's average-index formulas compare periods at a common frequency; mixing a full-year average base with a monthly target series would require an explicitly justified mixed-frequency extension. Rather than introduce an approximation, annual-average contribution bases are left out of this mode until that display and interpretation are specified.

## Reference

Patrick Schwind and Sebastian Weinand (2026), “Contributions to euro area inflation over arbitrary time periods”, 19th Meeting of the Ottawa Group on Price Indices.
