# Technical specification

This project is built with (major ecosystems):

- R, 3.4.2 (under `packrat`)
- tidyverse
- shiny
- shinydashboard
- plotly
- leafletjs

# Data collection

## Special educational needs

Available at [Statistics: special educational needs](
https://www.gov.uk/government/collections/statistics-special-educational-needs-sen).

>     send_data
>     ├── SEN-2011
>     │   └── SFR14_2011_UD.csv
>     ├── SEN-2012
>     │   └── SFR14_2012_UD.csv
>     ├── SEN-2013
>     │   └── SFR30-2013_UD.csv
>     ├── SEN-2014
>     │   └── SFR26_2014_UD.csv
>     ├── SEN-2015
>     │   └── SFR25_2015_UD.csv
>     ├── SEN-2016
>     │   └── SFR29_2016_UD.csv
>     ├── SEN-2017
>     │   └── SFR37_2017_UD.csv
>     └── SEN-2018
>         └── SEN_2018_Underlying_Data.csv

The main source of data.

## Edubase

>     edubase
>     └── edubasealldata20181022.csv

Available at
[Get information about schools](
https://get-information-schools.service.gov.uk/Downloads).

We use edubase data ("edubasealldata", retrieved on 2018-10-22)
for consistent labelling of classifications.

## Boundary files

Available at
[uk data service](
https://borders.ukdataservice.ac.uk/easy_download_data.html).

>     England_ct_2011
>     ├── england_ct_2011.dbf
>     ├── england_ct_2011.prj
>     ├── england_ct_2011.shp
>     ├── england_ct_2011.shx
>     └── TermsAndConditions.html
>     England_gor_2011_gen_clipped/
>     ├── england_gor_2011_gen_clipped.dbf
>     ├── england_gor_2011_gen_clipped.prj
>     ├── england_gor_2011_gen_clipped.shp
>     ├── england_gor_2011_gen_clipped.shx
>     └── TermsAndConditions.html
>     England_parl_2011_gen_clipped/
>     ├── england_parl_2011_gen_clipped.dbf
>     ├── england_parl_2011_gen_clipped.prj
>     ├── england_parl_2011_gen_clipped.shp
>     ├── england_parl_2011_gen_clipped.shx
>     └── TermsAndConditions.html
