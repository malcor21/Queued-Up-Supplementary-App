## Final Project Repo Setup

## Overview

This subdirectory contains the imported and modified data files used in this project.

## Table of Contents

### Sub-directories

- [`non-ISO`](non-ISO): contains the final grid regions shapefile, `non_ISO_final_v2.shp`,
and auxilliary files as well as previous shapefile versions for recordkeeping
- [`queues_import`](queues_import): contains the original dataset and codebook `.csv` files from the "Queued Up 2024" report[^1] 
- [`RTO_regions`](RTO_regions): contains the RTO regions shapefile used to synthesize the final grid regions shapefile[^2]
- [`tl_2024_us_state`](tl_20224_us_state): contains the US states shapefile used to synthesize the final grid regions shapefile[^3]

### Files

- `intq.rda`: cleaned .rda file used for ShinyApp data visualizations

## Contact Information

This project was completed for a Northwestern University course on the Data
Science Minor track of the Program in Statistics. The creator of this project
can be contacted at:

- Reed Malcolm, reedmalcolm2025@u.northwestern.edu

[^1]: "Queued Up: 2024 Edition, Characteristics of Power Plants Seeking Transmission Interconnection As of the End of 2023." From J. Rand, N. Manderlink, W. Gorman, R. Wiser, J. Seel, J. Kemp, S. Jeong, and F. Karhl, 2024. <https://emp.lbl.gov/queues>
[^2]: "RTO Region", U.S. Energy Information Administration, 18 February 2025. 
<https://atlas.eia.gov/datasets/eia::rto-regions/explore>
[^3]: "2024 TIGER/Line Shapefiles: States (and equivalent)", U.S. Census Bureau
Geography Division, 2024. <https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2024&layergroup=States+%28and+equivalent%29>