# library(two.laws.big.bang)
# library(two.laws.dev)

PackageData <- PackageDataClass$new("us-babynames")

ssa <- PackageData$GetHtmlTable(
  url = "https://www.ssa.gov/oact/babynames/numberUSbirths.html",
  indexTable = 2,
  colNames = c("Year", "Male", "Female", "Total")
)

ssa$Total <- NULL

ssa$Male <- parse_number(ssa$Male)
ssa$Female <- parse_number(ssa$Female)

us.births <- ssa %>%
  gather(Sex, Count, Male:Female) %>%
  arrange(Year, Sex) %>%
  mutate(Count = as.integer(Count))

write_csv(us.births, "data-raw/us-births.csv")
devtools::use_data(us.births, overwrite = TRUE)