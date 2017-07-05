# Hope-etal-Condor-Progression
Code associated with publication of analysis of the timing of migration for
Western Sandpipers and Dunlin using counts from 5 migratory stopover sites on
northward migration along the Pacific Flyway of North America.

## Getting Started

The scripts included are mostly for reference. The simulation scripts can be run without
further data, but the analysis scripts require the  original data, which is available from the
authors or in the data repositories described in the manuscript

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

Modify the `siteibm.ctl` control file to modify the paramaters for the model.


## Script Authors

* **David Hope** - *Coding and bug testing* - [David Hope](http://www.davidhope.ca)

Co-authors on the manuscript provided input into the methodology. Their information is
described in the text.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
