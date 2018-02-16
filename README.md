# Hope-etal-Condor-Progression
Code associated with publication of analysis of the timing of migration for
__Divergent trends in migration timing of shorebirds along the Pacific Flyway__.

## Getting Started

The scripts included are mostly for reference. The simulation scripts can be run without
further data, but the analysis scripts require the  original data, which is available from the
authors or in the data repositories described in the manuscript.

Publicly available data are found here:

1) [Roberts Bank, BC](http://donnees.ec.gc.ca/data/species/protectrestore/roberts-bank-shorebird-surveys-british-columbia/)

2) [Tofino Mudflats, BC](http://donnees.ec.gc.ca/data/species/protectrestore/tofino-mudflats-shorebird-surveys-british-columbia/?lang=en)

3) [Kachemak Bay, AK](http://kachemakbaybirders.org/blog/category/citizen-science/shorebird-monitoring/) 

Other data available by contacting data owners described in manuscript.

### Prerequisites

Python 3.4 and R 3.4

Some modules will need to also be install for each software.

R:
  tidyverse
  cowplot
  boot
  lme4
  lattice
  corrgram
  broom
  purrr
  gamlss
  gamlss.tr
  lazyeval

To install these packages from the R command line type:

```{r}
install.packages(c("tidyverse", "cowplot","boot","lme4","lattice","corrgram","broom","purrr","gamlss","gamlss.tr","lazyeval"))
```

Python:
  numpy
  pandas
  time
  itertools
  math



## Running the simulations

If you have the prerequisites you can run the python scripts from the command line by typing:

```{python}
python simPower.py

```

Modify the `siteibm.ctl` control file to modify the paramaters for the model. The output of the model can be analyzed using `Simulation2.r`. See the example code at the bottom of this script.

### Power analysis

The power analysis can be run using in R using `R source("Simulation1.r")`. To recreate the data from scratch, you will need to uncomment lines 51-57 and run the simulations again.

### Analysis of peak passage

The final analysis presented in __Hope et al. Migratory Progression of Shorebirds__ can be run using `Post-hoc_analysis.R`. The data is imported from rds files. The estimated peak passaged dates are described in detail in Supplementary Table S1 of the paper. The function used to calculate the dates of peak passage is shown in `muEstimation.R`. Data cleaning and modifications from raw data are shown in `data.clean.r`, except for Copper River and Hartney data, which are shown in `CRD_HART_data.r`.


## Script Authors

* [**David Hope**](http://www.davidhope.ca) - *Coding and bug testing* - [David Hope]
* **Mark Drever** - *Interpolation of species composition* - `HART_species_composition.R` and `CRD_species_composition.R`

Co-authors on the manuscript provided input into the methodology. Their information is
described in the text.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
