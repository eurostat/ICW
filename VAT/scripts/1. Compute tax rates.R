library(XLConnect)
library(tidyverse)
library(chron)
library(reshape2)

tempFile <- tempfile()
download.file("https://github.com/eurostat/ICW/blob/master/VAT/data/vat_rates_en.xls", destfile = tempFile)

## alternatively, download the file, directly from the EC website
#download.file("https://ec.europa.eu/taxation_customs/sites/taxation/files/resources/documents/taxation/vat/how_vat_works/rates/vat_rates_en.xls", destfile = tempFile)

reducedVAT <- readWorksheetFromFile(tempFile, sheet = "Reduced VAT rates in Annex III", startRow = 4)

reducedVAT <- reducedVAT %>%
  fill(Col1, .direction = "down") %>%
  fill(Category, .direction = "down")

reducedVAT <- reducedVAT[1:75, ]

rdVAT <- melt(reducedVAT, id.vars = c("Col1","Category"))
rdVAT <- filter(rdVAT,
                !is.na(value))

rdVAT <- rdVAT[!duplicated(paste0(rdVAT$variable, rdVAT$Col1)),]
rdVAT <- mutate(rdVAT,
                value = gsub(",", ".",value))

## current VAT rates

VATrates <- readWorksheetFromFile(tempFile, sheet = "List of VAT rates applied", startRow = 3, startCol = 5)
VATrates <- mutate(VATrates,
                   superRed = as.numeric(gsub(",", ".", Super.reduced.Rate)),
                   std = as.numeric(gsub(",", ".", Standard.Rate)),
                   Reduced.Rate = gsub(",", ".", Reduced.Rate))
completeVector <- function(v,l) {
  if (length(v) < l)
    v <- c(v, rep(NA, l - length(v)))
  return(v)
}
rdRt <- strsplit(VATrates$Reduced.Rate, "/")
lmax <- max(unlist(lapply(rdRt, length)))
completeV <- function(v) {
  completeVector(v, lmax)
}
rdRt <- lapply(rdRt, completeV)

rdRt <- do.call("rbind", rdRt)
VATrates$red1 <- as.numeric(rdRt[,1])
VATrates$red2 <- as.numeric(rdRt[,2])

## determine what is zero rate/super-reduced rate/reduced rate/standard rate

rdVAT <- merge(rdVAT, VATrates[, c(1,6:9)], by.x = "variable", by.y = "Code")

rdVAT <- mutate(rdVAT,
                rate = as.numeric(value),
                VAT_rate = ifelse(substr(value,1,4) == "[ex]", "EXEMP",
                                  ifelse(value == "0", "ZERO",
                                         ifelse(!is.na(rate) & !is.na(superRed) & rate == superRed, "SUPER",
                                                ifelse(!is.na(rate) & !is.na(red1) & rate == red1, "RED1",
                                                       ifelse(!is.na(rate) & !is.na(red2) & rate == red2, "RED2",
                                                              ifelse(!is.na(rate) & rate == std, "STD", "STD")))))))
