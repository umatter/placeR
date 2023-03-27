
# placeR <a href="https://umatter.github.io/placeR/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->
[![Experimental](https://img.shields.io/badge/status-experimental-orange)](https://github.com/umatter/placeR)
<!-- badges: end -->

A package to easily search and retrieve data from the Google Places API.

## Installation

You can install the development version of placeR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("umatter/placeR")
```

## Usage

Load the package and registeer the Google Places API key.

```R
library(placeR)
api.key <- "YOUR-API-KEY"
api_key(api.key)
```

Search restaurants within a 5km radius around specific coordinates with `searchNearby()`:

```R
nyrest <- searchNearby(location = "40.7,-74.0", radius = 5000, types = "restaurant")
```

3. Extract and inspect the data using `placesData()`:

```R
ny <- placesData(nyrest)
dim(ny)
str(ny)
ny[1:3, 1:4]
```

4. Plot the results using the plot method for objects of class "`placesearch`":

```R
library(ggmap)
register_google(api.key)
plot(nyrest, ggmap.zoom = 12)
```
