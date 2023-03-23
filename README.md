
# placeR

<!-- badges: start -->
[![Experimental](https://img.shields.io/badge/status-experimental-orange)](https://github.com/umatter/placeR)
<!-- badges: end -->

The goal of placeR is to ...

## Installation

You can install the development version of placeR from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("umatter/placeR")
```

## Usage

1. Store your Google Places API key in a variable `api.key`:

```R
api.key <- "YOUR_API_KEY"
```

2. Search restaurants within a 5km radius around specific coordinates with `searchNearby()`:

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
