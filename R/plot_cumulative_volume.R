#' Plot cumulative volume of data for the specific time period
#'
#' @param objects (data.frame) Table obtained from `query_objects`
#' @param from Start date of plot (chatacter or POSIXct)
#' @param to End date of plot (character of POSIXct)
#' @param ... additional arguments to `plot_theme_adc`
#'
#' @return Plot of total data volume
#' @export
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data
plot_cumulative_volume <- function(objects,
                                   from = as.Date("2009-01-01"),
                                   to = as.Date(Sys.Date()),
                                   ...) {
    
    if (class(from)[1] == "character"){
        from <- as.Date(from)
    }
    if (class(to)[1] == "character"){
        to <- as.Date(to)
    }
    
    # Plotting variables
    adc_launch <- as.Date(lubridate::ymd("20160405", tz = "America/Los_Angeles"))
    plot_start <- as.Date(lubridate::ymd("20160401", tz = "America/Los_Angeles"))
    
    options(scipen=999)
    
    adc_sizes <- objects %>% 
        dplyr::filter(is.na(.data$obsoletedBy)) %>%
        dplyr::filter(!is.na(.data$dateUploaded)) %>%
        dplyr::filter(.data$dateUploaded >= as.Date(from) & .data$dateUploaded <= to) %>%
        dplyr::mutate(dateUploaded = as.Date(.data$dateUploaded),
               size_kb = as.numeric(.data$size)/1024) %>%
        dplyr::group_by(.data$dateUploaded, .data$formatType) %>%
        dplyr::summarise(size_kb = sum(as.numeric(.data$size_kb))) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(.data$dateUploaded) %>%
        dplyr::mutate(cumsize = cumsum(.data$size_kb))
    
    repo_size <- round(max(adc_sizes$cumsize)/1e9, 1)
    
    adc_sizes_2017 <- adc_sizes %>%
        dplyr::filter(dateUploaded > ISOdate(2017, 1, 1, tz=Sys.timezone()))
    fit <- lm(adc_sizes_2017$cumsize ~ adc_sizes_2017$dateUploaded)
    time_range <- as.numeric(max(adc_sizes_2017$dateUploaded) - min(adc_sizes_2017$dateUploaded), units="days")/(365)
    size_range <- udunits2::ud.convert(max(adc_sizes_2017$cumsize) - min(adc_sizes_2017$cumsize), "kilobyte", "terabyte")
    avg_change <- round(size_range/time_range, 2) # in units TB/yr
    
    # Setup our axis labels
    date_axis_min <- trunc(as.Date(min(adc_sizes$dateUploaded)), units = c("years"))
    date_axis_max <- trunc(as.Date(max(adc_sizes$dateUploaded)), units = c("years"))
    date_axis_breaks <- seq(date_axis_min, date_axis_max, by = "years")
    
    # Plot total repository size over time
    g <- ggplot(adc_sizes, aes(x = dateUploaded,
                          y = cumsize/1e9)) +
        geom_line(size = 1.1, color="#1D244F") +
        geom_point(size=0.8, aes(y=size_kb/1e9), stroke=0, color="firebrick", alpha=0.5) +
        geom_line(data = fortify(fit), aes(x = adc_sizes_2017$dateUploaded, y = .fitted/1e9, linetype=NULL), color="firebrick") +
        annotate(geom = "text",
                 x =  as.Date(ymd("20200630", tz = "America/Los_Angeles")),
                 y = 50,#min(adc_sizes$cumsize) + 5.5,
                 angle = 0,
                 hjust = -0.15,#-0.075,
                 vjust = 1.9,
                 label = paste("Rate: ", avg_change, "TB/yr"),
                 color = "firebrick",
                 size = 3) +
        annotate(geom = "text",
                 x =  as.Date(ymd("20190601", tz = "America/Los_Angeles")),
                 y = repo_size,
                 angle = 0,
                 #hjust = -0.15,#-0.075,
                 #vjust = 1.9,
                 label = paste("Current: ", repo_size, "TB"),
                 color = "#146660",
                 size = 4) +
        scale_x_date(breaks = date_axis_breaks,
                     labels = as.character(year(date_axis_breaks))) +
        labs(x = "",
             y = "Repository Size (TB)") +
        plot_theme_adc(...) +
        theme(legend.title = element_blank(),
              legend.position = "none")
    
    # if (adc_launch > from & adc_launch < to){
    #     g <- g +
    #         ggplot2::geom_vline(xintercept = as.numeric(as.Date(lubridate::ymd("20160405", tz = "America/Los_Angeles"))), color = "#146660") +
    #         ggplot2::annotate(geom = "text",
    #                           x =  as.Date(lubridate::ymd("20160405", tz = "America/Los_Angeles")),
    #                           y = min_y,
    #                           angle = 90,
    #                           hjust = -0.15,#-0.075,
    #                           vjust = 1.9,
    #                           label = "ADC Launch (April 5, 2016)",
    #                           color = "#146660",
    #                           size = 3)
    # }
    
    
    return(g)
}
