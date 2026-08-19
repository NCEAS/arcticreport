#' Count number data downloads
#'
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#'
#' @return Number of downloads in given time period
#' 
#' @importFrom jsonlite fromJSON
#' 
#' @export
#'

count_downloads <- function(from = as.POSIXct("2010-01-01"), to = as.POSIXct(Sys.Date())){

    from <- as.Date(from); to <- as.Date(to)
    from_q <- paste(stringr::str_pad(lubridate::month(from), 2, side = "left", pad = "0"),
                    stringr::str_pad(lubridate::day(from), 2, side = "left", pad = "0"),
                    stringr::str_pad(lubridate::year(from), 2, side = "left", pad = "0"),
                    sep = "/")

    to_q <- paste(stringr::str_pad(lubridate::month(to), 2, side = "left", pad = "0"),
                    stringr::str_pad(lubridate::day(to), 2, side = "left", pad = "0"),
                    stringr::str_pad(lubridate::year(to), 2, side = "left", pad = "0"),
                    sep = "/")
    
    timeout_seconds <- 180
    options(timeout = timeout_seconds)

    d <- jsonlite::fromJSON(paste0("https://logproc-stage-ucsb-1.test.dataone.org/metrics?q=%7B%20%22metricsPage%22:%20%7B%20%22total%22:%200,%20%22start%22:%200,%20%22count%22:%200%20%7D,%20%22metrics%22:%20%5B%20%22downloads%22%20%5D,%20%22filterBy%22:%20%5B%20%7B%20%22filterType%22:%20%22repository%22,%20%22values%22:%5B%22urn:node:ARCTIC%22%5D,%20%22interpretAs%22:%20%22list%22%20%7D,%20%7B%20%22filterType%22:%20%22month%22,%20%22values%22:%20%5B%20%22", from_q,"%22,%20%22",to_q,"%22%20%5D,%20%22interpretAs%22:%20%22range%22%20%7D%20%5D,%20%22groupBy%22:%20%5B%20%22months%22%20%5D%7D"))

    return(as.character(sum(d$results$downloads)))

}
