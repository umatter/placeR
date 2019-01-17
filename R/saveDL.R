##' Save Download
##'
##' Handles download of API data via large number of requests
##' @usage saveDL(request.function, request.id, chunksize=100, pause=0, backupfile="saveDl.list.Rdata" )
##' @param request.function the request function used for the individual downloads
##' @param request.id list, the input to the request function
##' @param chunksize ingeger, number of cases in one chunk
##' @param pause numeric, duration of break between chunks in seconds
##' @param backupfile character, the path/name of the file to cache the data during download in progress
##' @url.list list of urls (character strings)
##' @return a list with the parsed json objects
##' @details an internal function for request functions to manage a large number of requests. the queries are split up and the responses saved on disk chunk by chunk
##' @author Ulrich Matter <umatter@protonmail.com>


saveDL <-
  function(request.function, request.id, chunksize=100, pause=0, backupfile="saveDl.list.Rdata" ) {

    n <- length(request.id)
    if (n <= chunksize) chunksize <- 1
    rest <- n%%chunksize

    chunks.upper <- seq(from = chunksize, to = n, by = chunksize)

    if (rest != 0) {

      chunks.upper[length(chunks.upper) + 1] <- chunks.upper[length(chunks.upper)] + rest

    }

    chunks.lower <- c(1,chunks.upper[-length(chunks.upper)] + 1)



    # prepare for loop over all chunks
    chunks <- data.frame(lower=chunks.lower, upper=chunks.upper)
    pb <- txtProgressBar(min = 0, max = nrow(chunks), style = 3)

    response.list <- as.list(1:nrow(chunks))
    save(response.list, file=backupfile) # to be saved and loaded in each loop

    # process queries chunkwise
    for (i in 1:nrow(chunks)) {

      Sys.sleep(pause)

      first <- chunks$lower[i]
      last <- chunks$upper[i]


      rIds <- request.id[first:last]

      response <- request.function(rIds)


      load(backupfile)
      response.list[[i]] <- response
      save(response.list, file=backupfile)
      rm(response.list )
      gc(, verbose=FALSE) # clean memory

      setTxtProgressBar(pb, i)

    }

    load(backupfile)
    response.list <- redlist(response.list)

    return(response.list)


  }
