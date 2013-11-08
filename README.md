cbrf.R
====

Currency converter for R using Russian Central Bank data API.
Depends on RCurl, XML and lubridate packages.


Usage:

    >> co = make.converter()
    >> co(ymd("2013-05-05"), 100, "EUR", "RUB")
    [1] 4062.64

Caches exchange rate data on disk in a temporary directory, or you can specify cache directory as a parameter to `make.converter()`

Warning
--------

As it uses floating point numbers all over, it is not recommended for anything besides home finances, use at your own risk.
