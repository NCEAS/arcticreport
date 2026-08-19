#' Count number of support interactions
#'
#' @param from Start date to count over (chatacter or POSIXct)
#' @param to End date to count over (character of POSIXct)
#' @param wd Working directory to read and write files to/from
#'
#' @return Number of support interactions
#' @export
#'

count_support_interactions <- function(from = as.POSIXct("2010-01-01"), to = as.POSIXct(Sys.Date()), wd = getwd()){

    from <- as.Date(from); to <- as.Date(to)
    from_q <- paste(stringr::str_pad(lubridate::month(from), 2, side = "left", pad = "0"),
                    stringr::str_pad(lubridate::day(from), 2, side = "left", pad = "0"),
                    stringr::str_pad(lubridate::year(from), 2, side = "left", pad = "0"),
                    sep = "/")

    to_q <- paste(stringr::str_pad(lubridate::month(to), 2, side = "left", pad = "0"),
                  stringr::str_pad(lubridate::day(to), 2, side = "left", pad = "0"),
                  stringr::str_pad(lubridate::year(to), 2, side = "left", pad = "0"),
                  sep = "/")

    year <- paste(lubridate::year(from), lubridate::year(to), sep = "|")

    paths <- dir(system.file("extdata", package = "arcticreport"), full.names = TRUE) %>%
        grep(year, ., value = TRUE)

    if (is.null(paths) || any(is.na(paths)) || length(paths) == 0){
      return(NA)
    }

    his <- lapply(paths, read.csv)

    his_full <- do.call(dplyr::bind_rows, his) %>%
        dplyr::mutate(ticket = as.character(ticket))

    tickets <- read.csv(system.file("extdata", "ticket_list.csv", package = "arcticreport")) %>%
        dplyr::rename(ticket = id) %>%
        dplyr::rename(ticket_created = Created) %>%
        dplyr::mutate(ticket = as.character(ticket))

    his <- dplyr::left_join(his_full, tickets, by = "ticket") %>%
        dplyr::filter(created >= from & created <= to)
    
    return(nrow(his))

}

#' Update text file of all tickets
#' @param path a path to write the ticket list to 
#'
#' @export
update_ticket_list <- function(path = system.file("extdata", "ticket_list.csv", package = "arcticreport")){
    tics <- rt_ticket_search("Queue='arcticdata'",
                             orderby = "+Created",
                             format = "l",
                             fields = "id,Created")

    tics_clean <- tics %>%
        mutate(Created = substr(Created, start = 5, stop = 24)) %>%
        mutate(Created = as.POSIXct(Created, format = "%b %d %H:%M:%S %Y"))

    #path <- system.file("extdata", , package = "arcticreport")
    print(paste("THIS IS THE PATH", path))
    write.csv(tics_clean, path, row.names = F)
    return(NULL)
}

# helper function to get individual ticket history
get_ticket_history <- function(ex){
    tmp <- rt_ticket_history(gsub("ticket/", "", ex), format = "l")
    t <- tempfile()

    writeLines(tmp$body, t)
    tmp2 <- scan(t, what = "char", sep = "\n", quiet = TRUE)

    inds <- grep("^-{2}", tmp2)
    inds <- c(1, inds, length(tmp2))
    events <- list()

    for (i in 1:(length(inds)-1)){
        start <- inds[i]
        end <- inds[i+1]
        events[[i]] <- tmp2[start:end]
    }

    events <- lapply(events, parse_event)
    events <- do.call(bind_rows, events) %>%
        filter(type == "Correspond")

    return(events)
}

# helper function to parse ticket evvents
parse_event <- function(x){
    type <- grep("Type:", x, value = T) %>%
        gsub("Type: ", "", .)
    creator <- grep("Creator:", x, value = T) %>%
        gsub("Creator: ", "", .)
    created <- grep("Created:", x, value = T) %>%
        gsub("Created: ", "", .) %>%
        trimws(., which = "both")
    ticket <- grep("^Ticket:", x, value = T) %>%
        gsub("Ticket: ", "", .)

    if (is.null(type)){
        type <- NA
    }

    if (length(type) == 1 & length(creator) == 1 & length(created) == 1 & length(ticket) == 1){
        result <- data.frame(ticket, type, creator, created, stringsAsFactors = F)
    }

    else {
        result <- data.frame(dump = paste0(ticket, type, creator, created, sep = ";", collapse = ";"), stringsAsFactors = F)
    }


}

#' Update text file of annual ticket events
#'
#' @param year Year to update
#' @param path path of ticket list to read from
#'
#' @export
#'
update_annual_tix <- function(year, path = system.file("extdata", "ticket_list.csv", package = "arcticreport")){
    #path <- system.file("extdata", "ticket_list.csv", package = "arcticreport")
    tics_df <- read.csv(path)

    tics_filt <- tics_df %>%
        filter(year(ymd_hms(Created)) == year)

    his <- list()
    for (i in 1:nrow(tics_filt)){
        his[[i]] <- get_ticket_history(tics_filt$id[i])
    }

    his_df <- do.call(bind_rows, his)

    fname <- paste0("~/arcticreport/inst/extdata/", year, "_ticket_events.csv")
   # path <- system.file("extdata", fname, package = "arcticreport")

    write.csv(his_df, fname, row.names = F)
    return(his_df)
}


