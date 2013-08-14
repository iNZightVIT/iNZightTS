print.iNZightTS <-
function(obj, full=FALSE) {
    writeLines("Current Variable:")
    print(obj$currVar)

    if (obj$freq > 1)
        writeLines(paste("\nTime Series:\nStart =",
                         paste(obj$start, collapse = ", "),
                         "\nEnd =",
                         paste(obj$end, collapse = ", "),
                         "\nFrequency =",
                         paste(obj$freq,  collapse = ", ")))
    else
        writeLines("\n")
    print(obj$tsObj)

    writeLines("\nData:")
    if (full)
        print(obj$data)
    else {
        print(head(obj$data))
        writeLines("...")
    }
}
