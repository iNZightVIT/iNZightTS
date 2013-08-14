print.iNZightTS <-
function(x, full = FALSE, ...) {
    writeLines("Current Variable:")
    print(x$currVar)

    if (x$freq > 1)
        writeLines(paste("\nTime Series:\nStart =",
                         paste(x$start, collapse = ", "),
                         "\nEnd =",
                         paste(x$end, collapse = ", "),
                         "\nFrequency =",
                         paste(x$freq,  collapse = ", ")))
    else
        writeLines("\n")
    print(x$tsObj)

    writeLines("\nData:")
    if (full)
        print(x$data)
    else {
        print(head(x$data))
        writeLines("...")
    }
}
