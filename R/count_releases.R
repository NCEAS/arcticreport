
get_closed_issues <- function(org, repo, date) {
    n <- 0
    
    for (i in 1:length(org)){
        issues <- gh::gh(paste0("GET /repos/", org, "/", repo, "/issues?state=all&since=",as.POSIXct(date),"&per_page=100&page=", as.character(i)))
        if (length(issues) > 0){
            for (z in 1:length(issues)){
                if (issues[[z]]$state == "closed"){
                    n <- n + 1
                }
            }
        } else if (length(issues) == 0) {
            break
        }
    }
    return(n)
    
}

get_releases <- function(org, repo, date) {
    n <- 0
    releases <- gh::gh(paste0("GET /repos/", org, "/", repo, "/releases"))
    if (length(releases) > 0){
        for (z in 1:length(releases)){
            if (as.Date(releases[[z]]$created_at) > as.Date(date)){
                n <- n + 1
            }
        }
    }
    
    return(n)
    
}

get_issues_and_releases <- function(from_date){
    repos <- dplyr::tribble(~org, ~repo, ~type,
                            "nceas", "metacat", "repository",
                            "nceas", "metacatUI", "search and editor",
                            "nceas", "metadig-engine", "quality",
                            "nceas", "metadig-checks", "quality",
                            "nceas", "metadig-webapp",  "quality",
                            "nceas", "metadig-py", "quality",
                            "nceas", "metadig-r", "quality",
                            "dataoneorg", "hashstore", "repository",
                            "dataoneorg", "hashstore-java", "repository",
                            "dataoneorg", "dataone-indexer", "repository",
                            "dataoneorg", "metrics-service", "metrics",
                            "nceas", "arcticdatautils", "curation tools",
                            "nceas", "awards-bot", "curation tools",
                            "nceas", "submissions-bot", "curation tools",
                            "nceas", "datamgmt", "curation tools",
                            "dataoneorg", "rdataone", "curation tools",
                            "ropensci", "datapack", "curation tools",
                            "dataoneorg", "scythe", "metrics", 
                            "dataoneorg", "sem-prov-ontologies", "repository",
                            "dataoneorg", "d1_python", "repository",
                            "dataoneorg", "speed-bagit", "repository"
    )
    
    
    repos_f <- repos %>%
        dplyr::mutate(
            closed_issues = purrr::pmap_chr(
                list(repo = repo, org = org, date = from_date), 
                ~ get_closed_issues(..2, ..1))) %>% 
        dplyr::mutate(closed_issues = as.integer(closed_issues)) %>% 
        dplyr::mutate(
            releases = purrr::pmap_chr(
                list(repo = repo, org = org, date = from_date), 
                ~ get_releases(..2, ..1))) %>% 
        dplyr::mutate(releases = as.integer(releases))
    
}


