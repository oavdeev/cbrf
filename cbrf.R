# Currency converter using Russian Central Bank web API.
# Depends on RCurl, XML and lubridate packages.
#
# Usage: 
#  >> co = make.converter()
#  >> co(ymd("2013-05-05"), 100, "EUR", "RUB")
#  [1] 4062.64
#
# Caches exchange rate data on disk in a temporary directory, or you can 
# specify cache directory as a parameter to make.converter()
#

library("XML")
library("RCurl")
library("lubridate")

make.converter <- function(cache.dir = NULL) {
  cache.dir <- if (is.null(cache.dir)) tempdir() else cache.dir

  getCurrencyList = function() {
    today <- Sys.Date()
    files <- dir(path=cache.dir, pattern=paste("currency-",".*",".xml", sep=""))
    cache.filename <- paste("currency-",format(today, format="%Y%m"),".xml",sep="")
    
    if (length(files)) {
      f <- file.path(cache.dir, files[1])
      x <- paste(readLines(f), collapse=" ")
      xx <- xmlParse(x)
    } else {
      h <- basicTextGatherer()
      body = '<?xml version="1.0" encoding="utf-8"?>
      <soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">
      <soap12:Body>
      <EnumValutesXML xmlns="http://web.cbr.ru/">
      <Seld>false</Seld>
      </EnumValutesXML>
      </soap12:Body>
      </soap12:Envelope>'
      curlPerform(url = "http://www.cbr.ru/DailyInfoWebServ/DailyInfo.asmx",
                  httpheader=c(Accept="text/xml", 'Content-Type' = "application/soap+xml; charset=utf-8"), 
                  postfields=body, writefunction = h$update, verbose = TRUE)
      xx <- xmlParse(h$value())
      cat(paste(h$value(),"\n"), file=file.path(cache.dir, cache.filename))
    }
    
    # returns string w/o leading or trailing whitespace
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    cur <- list()
    for (n in getNodeSet(xx, "//EnumValutes")) {
      children <- xmlChildren(n)
      cc <- xmlValue(children$VcommonCode)
      code <- xmlValue(children$VcharCode)
      cur[[code]] <- trim(cc)
    }
    return (cur)
  }
  
  getYearData <- function (year, cur) {
    xml_parse <- function (n) {
      date = xmlGetAttr(n, "Date")
      children <- xmlChildren(n)
      nominal <- xmlValue(children$Nominal)
      value <- xmlValue(children$Value)
      return(list(date=date, nominal=nominal, value=value))
    }
    url <- paste(
      "http://www.cbr.ru/scripts/XML_dynamic.asp?",
      "date_req1=", "01/01/", year, 
      "&date_req2=", "31/12/", year,
      "&VAL_NM_RQ=", cur,
      sep="")
    today <- Sys.Date()
    files <- dir(path=cache.dir, pattern=paste(year, cur,".*",".xml", sep=""))
    cache.filename <- paste(year,cur,".",today,".xml",sep="")
    
    if (length(files) && !(year == year(today) && !(cache.filename %in% files))) {
      if (file.exists(file.path(cache.dir, cache.filename))) {
        f <- file.path(cache.dir, cache.filename)
      } else {
        f <- file.path(cache.dir, files[1])
      }
      x <- paste(readLines(f), collapse=" ")
      xx <- xmlParse(x)
    } else {
      x <- getURLContent(url)
      xx <- xmlParse(x)    
      cat(x, file=file.path(cache.dir, cache.filename))
    }
    
    ll <- lapply(getNodeSet(xx, "//Record"), xml_parse)
    df <- do.call(rbind.data.frame, ll)
    df$date <- parse_date_time(as.character(df$date), "dmy")  
    df$value <- as.numeric(gsub(",", ".", as.character(df$value)))
    df$nominal <- as.numeric(gsub(",", ".", as.character(df$nominal)))
    return (df)
  }
  
  getYearDataCached <- function(yyyy, code) {
    key <- paste(as.character(yyyy), code, sep="")
    if (is.null(year_cache[[key]])) {
      res <- getYearData(yyyy, code);
      year_cache[[key]] <<- res;
      return (res);
    } else {
      return (year_cache[[key]]);
    }
  }
  
  getRatio <- function(date, cur) {
    yyyy <- year(date)
    code <- cur_codes[[cur]]
    if (is.null(code)) {
      stop("Unknown currency code ", cur)
    }
    yd <- getYearDataCached(yyyy, code)
    i <- findInterval(date, yd$date)
    if (i == 0) {
      yd <- getYearDataCached(yyyy-1, code)
      i <- findInterval(date, yd$date)
    }
    return (yd[i,]$value / yd[i,]$nominal)
  }

  year_cache <- list()
  cur_codes <- getCurrencyList()
  
  f <- function(date, amount, cur1, cur2) {
    if (cur2 == "RUB" && cur1 == "RUB") {
      return (amount);
    }
    if (cur2 == "RUB") {
      return (amount * getRatio(date, cur1))
    }
    if (cur1 == "RUB") {
      return (amount / getRatio(date, cur2))
    }
    return (amount * getRatio(date, cur1) / getRatio(date, cur2));
  }
  
  return (f);
}
