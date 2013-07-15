### read.infoshare reads in a CSV/Excel file from the Stats NZ infoshare website.
### It makes a data frame and sets the start value and frequency for ts
### (time series) objects. Currently only the data frame is returned.

read.infoshare = function(file.loc) {

    dat = read.csv(file.loc, header = TRUE, skip = 1, as.is = TRUE,
                   na.strings = c("NULL","NA","N/A","#N/A","","<NA>"), check.names = TRUE)
    names(dat)[1] = "time"

    ### The stats NZ files have some info at the end and this
    ### has been read into our data frame - so remove it
    cutOff = with(dat, which(time == "Table information:"))
    selectRow = rep(TRUE, nrow(dat))
    selectRow[cutOff:nrow(dat)] = FALSE
    dat = subset(dat, selectRow)

    ### We need to find the first row that numbers start at
    num.patt = "^[0-9]+(\\.[0-9]+)?$"
    i = 1
    while (length(grep(num.patt, dat[i,2])) == 0)
        i = i + 1
    numbers.start = i


    ### What frequency is the data at (monthly, quarterly, yearly)
    firstTime = dat$time[numbers.start]
    if (nchar(firstTime) > 4) {
        interval = substring(firstTime, 5, 5)
        freq = ifelse(interval == "Q", 4, 12)
    }
    else {
        interval = "A"
        freq = 1
    }

    ### form the start value (used when we create ts objects)
    start = as.numeric(substring(firstTime, 1 ,4))
    if (interval != "A") {
        start = c(start, as.numeric(substring(firstTime, 6)))
    }

    # Just return the data for now
    dat
}

