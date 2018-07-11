#' hidden crawler
#' 
#' crawlt den Inhalt einer Website und gibt den passenden xml-Output zurück
#' Der Crawl über die eigene IP oder das TOR-Netzwerk gestartet werden.
#' @param url zu crawlende url
#' @param ua zu verwendender user agent
#' @param proxy hidden=T, verwendeter socks port, um TOR-Browser anzusprechen
#' @param hidden crawl mit eigener (hidden=F) oder TOR-IP (hidden=T)
#' @param info anzeige welche IP verwendet wird (T/F)
#' @param verbose Ausgabe zusätzlicher Informationen zum Crawl (T/F)
#' @param timeout Zeit, in der auf Rückmeldung von Website gewartet wird; vor Verbindungsabbruch
#' @param encoding verwendeter Zeichensatz der Website
#' @param wait Wartezeit vor Crawl in Sekunden 
#' @param try.con Anzahl der gescheiterten Verbindungsversuche mit TOR, bis eigene IP verwendet wird
#' @param ssl ssl.verifypeer (T/F)
#' @return XML-Inhalt der aufgerufenen url
#' @export

hidden_crawl <- function(url,
                         ua="",
                         proxy="socks5h://127.0.0.1:9153",
                         hidden=T,
                         info=F,
                         verbose=F,
                         timeout=5,
                         encoding="UTF-8",
                         wait=0,
                         try.con=10,
                         ssl=F){
    # packages: RCurl, rvest, magrittr,
    Sys.sleep(wait) # Pause vor jedem Crawl
    webpage <- NA
    # Optionen für Crawl über TOR
    opt <- list(proxy=proxy,
                useragent=ua,
                followlocation=TRUE,
                referer="",
                timeout=timeout,
                verbose=verbose,
                ssl.verifypeer=ssl)
    # Optionen für Crawl mit eigener IP
    opt.unhide <- list(followlocation=TRUE,
                       referer="",
                       timeout=timeout,
                       verbose=verbose,
                       ssl.verifypeer=ssl)
    if (hidden == TRUE){ # Crawl über TOR-Netzwerk (wenn möglich)
        con.tries <- 0
        #Verbindungsversuche bis threshold erreicht/Verbindung erfolgreich
        while ((length(webpage)==1) & (con.tries < try.con)){
            con.tries <- con.tries + 1
            webpage <- tryCatch({
                webpage <- RCurl::getURL(url=url,.opts=opt) %>%
                    xml2::read_html(encoding=encoding)
            }, error=function(e){
                webpage <- NA
            })
        }
        if (length(webpage)==2){ # TOR-Verbidung erfolgreich
            if (info == TRUE) cat(" # TOR-IP\n")
        } else { # TOR-Verbindung fehlgeschlagen, eigene IP verwenden
            if (info == TRUE) cat(" # Eigene IP\n")
            webpage <- RCurl::getURL(url=url,.opts=opt.unhide) %>%
                xml2::read_html(encoding=encoding)
        }
    } else { # eigene IP für Crawl verwenden
        if (info == TRUE) cat(" # Eigene IP\n")
        webpage <- RCurl::getURL(url=url,.opts=opt.unhide) %>%
            xml2::read_html(encoding=encoding)
    }
    return(webpage)
}


x <- hidden_crawl(url="https://www.r-bloggers.com/how-to-make-and-share-an-r-package-in-3-steps",
                  hidden=F)
