# Boxplots of mean hourly percent by (some) industries
gender = read.csv('gender-data.csv')
gender$sicShort = sapply(gender$SicCodes, function (s) substr(s, start = 1, stop = 2))
industryList = list("agriculture" = paste0(c("01","02","03","04","05")), 
                    "manufacturing" = paste0(c(10:12, 15:19, 24:33, 35:37, 39)),
                    "construction" = paste0(c(40:42, 44)),
                    "logistics" = paste0(45:47),
                    "utilities" = paste0(48:49),
                    "resale" = paste0(c(50:57, 59, 60:65, 69)),
                    "finance/realestate" = paste0(c(70:76)),
                    "business service" = paste0(c(77)),
                    "government service" = paste0(c(81:86)),
                    "tourism" = paste0(91:92),
                    "Other service" = paste0(96:99))

classifyInd = function(sic, indList) {
  out = NA
  for (ind in names(indList)) {
    indCodes = indList[[ind]]
    if (is.element(sic, indCodes)) {
      out = ind
    }
  }
  return(out)
}

gender$industry = sapply(gender$sicShort, function (x) classifyInd(x, industryList))
boxplot(DiffMeanHourlyPercent~industry, data = gender, ylim = c(-50,70))
