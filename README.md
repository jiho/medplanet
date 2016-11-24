# MEDPLANET data

CARE catches from the N-W Mediterranean, from the projects:

- GIREL
- CasCioMar
- Embiez
- RESPIRE
- SUBLIMO
- RadeICHTHYO (Villefranche)

## Description of the data and code

`apps`: stub for a shiny app to visualise the dynamics of the data. Currently only a map.

`data`: repository for data from various projects

`explore.R`: old stuff

`moon_computation.R`: code to explore the computation of moon phase by package `oce` 

`prepare_data.R`: read and concatenate data, homogenise data, compute CPUE

`report.Rmd`: exploratory analyses, mostly at the level of the whole coastline.


## Outline of the analysis

All analyses should be performed (when appropriate):

-   on raw (or transformed) numerical data
-   on numerical data using techniques allowing to focus on exceptional abundances
-   on semi-quantitative data

The analyses should be performed on the whole community and on a selection of interesting/representative taxa.

1. Description of data
    - [x] spatial scope
    - [x] temporal scope
    - [x] sampling effort
    - [x] data distribution

2. Taxonomic composition
    - [x] which species/genus/families are more abundant
    - [ ] does that represent adult populations

3. Spatial variability
    - [x] abundance among sites
    - [-] species composition among sites
    - [ ] synchronicity among sites
    
4. Temporal variability
    - [ ] temporal scale of variation in abundance
    - [ ] seasonality
    - [ ] inter-annual variation in abundance
    - [ ] inter-annual consistency in seasonality
    - [ ] inter-annual variation in species composition
        
5. Forcing variables
    - [ ] local site variables (substrate type, depth, etc.)
    - [ ] moon phase (via moon light?)
    - [ ] local wind and wind history
    - [ ] regional wind regime
    - [ ] local temperature and temperature history
    - [ ] regional current
    - [ ] local plankton abundance
    - [ ] regional productivity
