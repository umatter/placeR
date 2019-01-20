##' Get Details Of Place
##'
##' Takes the reference code of a place and returns detailed information about this place.
##' @usage placeDetails(reference, language=NULL, saveDL=FALSE, chunksize=100, pause=0 )
##' @param reference character, the Google Places API reference code of a place (i.e. a restaurant).
##' @return a placedetails object
##' @details see https://developers.google.com/places/documentation/details for a list of all variables
##' @author Ulrich Matter <umatter@protonmail.com>
##' @example
##'placeDetails("CmRYAAAAciqGsTRX1mXRvuXSH2ErwW-jCINE1aLiwP64MCWDN5vkXvXoQGPKldMfmdGyqWSpm7BEYCgDm-iv7Kc2PF7QA7brMAwBbAcqMr5i1f4PwTpaovIZjysCEZTry8Ez30wpEhCNCXpynextCld2EBsDkRKsGhSLayuRyFsex6JA6NPh9dyupoTH3g")
##' # typical usage example (Basel)
##' # 1. get references to restaurants via radar search:
##' refs <- searchRadar(location="47.566667,7.6", radius=2000, types="restaurant")
##' # 2. get details to all these references:
##' rest.basel <- placeDetails(reference=refs$reference)
##' @export
##'


placeDetails <-
  function(reference, language=NULL, saveDL=FALSE, chunksize=100, pause=0 ) {

    urls <- generateParametersDetails(reference, language)

    if (saveDL==FALSE) {
      responses <- detailsRequest(url.list=urls)
    } else {
      responses <- saveDL(request.function=detailsRequest, request.id=urls, chunksize=chunksize, pause=pause )
    }

    results.df <- extractDetailsResults(x=responses)
    parameters.df <- data.frame(reference=unlist(reference),
                                language=ifelse(is.null(language),
                                                NA,
                                                language))

    resp <- new("placedetails", search.results = results.df, parameters = parameters.df)
    return(resp)
  }

