#' get_ua
#' 
#' ruft eine Liste mit user agents ab
#' @param url Website mit Liste an user agents
#' @param browser Browser für die user agents abgefragt werden sollen
#' @return Liste mit user agents
#' @export
get_ua <- function(url="https://developers.whatismybrowser.com/useragents/explore/software_name/",
                   browser=c("chrome","firefox")){
    # Liste mit unterschiedlichen User Agents einlesen
    # packages: magrittr, rvest
    ua.list <- list()
    for (i in browser){
        ua.list <- xml2::read_html(paste0(url,i)) %>%
            rvest::html_nodes(css=".useragent a") %>%
            rvest::html_text(trim=T) %>%
            append(ua.list)
    }
    return(unlist(ua.list))
}
