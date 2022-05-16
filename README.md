
# VizumApp

## A Shiny App for visualising uncertainty in spatial data using the Vizumap R package.

## Installation and Running

You can install a development version of the `Vizumap` package using the
command below.

    remotes::install_github(repo = "SamNelson081/VizumApp")

    library(VizumApp)
    runShiny()

## Authors

Sam Nelson, CSIRO’s Data61, Email: <Sam.Nelson@data61.csiro.au>

Lydia Lucchesi, Australian National University & CSIRO Data61, Email:
<Lydia.Lucchesi@anu.edu.au>

Petra Kuhnert, CSIRO’s Data61, Email: <Petra.Kuhnert@data61.csiro.au>

## About the package

This package builds a Shiny App version of the Vizumap R package that
appears [here](https://github.com/lydialucchesi/Vizumap). The app offers
four visualisations for visualising uncertainty calculated from spatial
data on a map. These visualisations are outlined in Lucchesi et
al. (2021).

This app demonstrates the visualisations using two case studies. The
first is a US case study described in Lucchesi and Wikle (2017), while
the second is based on uncertainty quantification methods developed in
Kuhnert et al. (2018) and demonstrated in Lucchesi et al. (2021). Users
can select either case study and explore the four different approaches
and tune their final visualisation according to the level of
transparency and colour scheme required.

## Visualising your own data

VizumApp can be used to visualise your own spatial data with
uncertainties. This requires a shape file for the region of interest and
a .csv file containing the predictions, uncertainties and OBJECTID that
links with the OBJECTID in the shapefile. For this version of the app
only, we require the label corresponding to the linked ID to be named
`OBJECTID`.

An example of what these files need to look like, see the `Burdekin_Ex`
directory in `inst/shinyApp/extdata`. To read in the shapefile you will
need to select all 4 files (UB.dbf, UB.prj, UB.shp and UB.shx). The
`.csv` file is read in separately. Once read in, you are ready to select
a visualisation.

To illustrate how to do this, watch this short
[video](https://youtu.be/LkX59z99Bo4).

## Link to VizumApp

A link to the shiny instance appears
[here](https://shiny.csiro.au/VizumApp/) but you can also run the app
locally after install by typing the following

    library(VizumApp)
    runShiny()

## Things to do

The following is not an exhaustive list. We plan to implement options
for:

-   Downloading and saving your final map.
-   Outputting a script that allows you to copy into an R Markdown
    document or directly into R.
-   Allowing the user to specify the column ID that links the estimates
    to the shapefile.
-   Selecting the distribution and probability threshold for the
    excedance map. Currently this is hard wired as an exponential
    distribution and probability of 0.8.

## Contribute

To contribute to `VizumApp`, please follow these
[guidelines](CONTRIBUTING.md).

Please note that the `VizumApp` project is released with a [Contributor
Code of Conduct](CONDUCT.md). By contributing to this project, you agree
to abide by its terms.

## License

`VizumApp` version 0.9.2 is licensed under [GPLv3](LICENSE.md).

## Citation

Nelson, S., Lucchesi, L. and Kuhnert. P.M. (2022). VizumApp: A Shiny App
for visualizing uncertainty in spatial data using the Vizumap R package,
DOI: <http://hdl.handle.net/102.100.100/439688?index=1>

## References

Lucchesi, L.R., Kuhnert, P.M. and Wikle, C.K. (2021) [Vizumap: an R
package for visualising uncertainty in spatial
data](https://doi.org/10.21105/joss.02409), Journal of Open Source
Software, <https://doi.org/10.21105/joss.02409>.

Kuhnert, P.M., Pagendam, D.E., Bartley, R., Gladish, D.W., Lewis, S.E.
and Bainbridge, Z.T. (2018) [Making management decisions in face of
uncertainty: a case study using the Burdekin catchment in the Great
Barrier Reef, Marine and Freshwater
Research](https://publications.csiro.au/publications/#publication/PIcsiro:EP168206),
69, 1187-1200, <https://doi.org/10.1071/MF17237>.

Lucchesi, L.R. and Wikle C.K. (2017) [Visualizing uncertainty in areal
data with bivariate choropleth maps, map pixelation and glyph
rotation](http://faculty.missouri.edu/~wiklec/LucchesiWikle2017Stat),
Stat, <https://doi.org/10.1002/sta4.150>.
