library(two.laws.big.bang)
library(two.laws.dev)

PackageData <- PackageDataClass$new("us-postsalcodes")


us.postalcodes <- rio::import("data-raw/us-postalcodes-data/postalcodes.csv.zip") %>%
  filter(UniqueZIPName == "Y") %>%
  select(
    one_of(
      "ZipCode",
      "Longitude",
      "Latitude",
      "State",
      "StateFullName",
      "City",
      "County",
      "Region",
      "CBSA_Name",
      "TimeZone"
      )
  ) %>%
  rename(
    PostalCode = ZipCode,
    State2 = State,
    State = StateFullName,
    Area = CBSA_Name
  ) %>% mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude)
  )


rio::export(us.postalcodes, "us-postalcodes.csv")
devtools::use_data(us.postalcodes, overwrite = TRUE)