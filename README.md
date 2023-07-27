# Exploring adoption of new AdaptWest climate data into the species habitat models

## Overview 

In the **ABMI** species habitat models, we include a set of climate covariates to explain residual variation in our landcover based models. This climate data was created by Diana Stralberg by interpolating existing climate data created using the parameter-elevation regressions on independent slopes model (PRISM) and down-scaled to a 500-m resolution using the Climate WNA tool. As we are reviewing our modeling framework, we decided to explore alternative climate data sources that would provide the following benefits:

- Use more recent climate normal data that matches when **ABMI** surveys occurred (e.g., 1991-2020 climate normal).
- Provides us with more climate variables that could be incorporated into our models.
- Expands the spatial extent so we can use a single source of climate data for analyses both within and outside of Alberta.
- Acquire data from a source that also have climate projections to support any future climate change work.
- Create down-scaled predictions for any survey locations monitored by the **ABMI**.

The full details outlining our assessment, and our new process for acquiring climate data can be found at the associated GitHub page (https://abbiodiversity.github.io/ClimateData/).

## Acknowledgements

We would like to acknowledge **AdaptWest** and **Daymet**for providing the climate data used in this analysis, as well as the **ClimateNA_v7.40** software for creating down-scaled predictions.

## Suggested citation

Alberta Biodiversity Monitoring Institute. 2023. Exploring adoption of new AdaptWest climate data into the species habitat models. Alberta Biodiversity Monitoring Institute, Alberta, Canada. (https://abbiodiversity.github.io/ClimateData/) 

AdaptWest Project. 2022. Gridded current and projected climate data for North America at 1km resolution, generated using the ClimateNA v7.30 software (T. Wang et al., 2022). Available at adaptwest.databasin.org.

Wang T, Hamann A, Spittlehouse D, Carroll C (2016) Locally Downscaled and Spatially Customizable Climate Data for Historical and Future Periods for North America. PLoS ONE 11(6): e0156720. doi:10.1371/journal.pone.0156720

Thornton, M.M., R. Shrestha, Y. Wei, P.E. Thornton, S-C. Kao, and B.E. Wilson. 2022. Daymet: Annual Climate Summaries on a 1-km Grid for North America, Version 4 R1. ORNL DAAC, Oak Ridge, Tennessee, USA. https://doi.org/10.3334/ORNLDAAC/2130

## Contact

For any questions regarding the contents of this repository, please contact Brandon Allen at brandon.allen@ualberta.ca.
