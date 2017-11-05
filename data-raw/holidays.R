library(two.laws.big.bang)

Years = 1970:2050


# Create some holiday functions for those not available in timeDate

Halloween <- function(year) {
  timeDate::timeDate(
    as.character(
      year * 10000 + 1031
    )
  )
}


# This needs to change to last friday of month
USOshaLogFile <- function(year) {
  timeDate::timeDate(
    as.character(
      year * 10000 + 0430
    )
  )
}


# This needs to change to first friday of month
USOshaLogPost <- function(year) {
  timeDate::timeDate(
    as.character(
      year * 10000 + 0201
    )
  )
}


USDaylightSavingsBegins <- function(year){

  NewYork <- timeDate::New_York()
  Begins <- as.Date(NewYork$New_York[NewYork$isdst == 0])

  return(
    as.Date(Begins[between(lubridate::year(Begins), min(year), max(year))])
  )
}

USDaylightSavingsEnds <- function(year){

  NewYork <- timeDate::New_York()
  Ends <- as.Date(NewYork$New_York[NewYork$isdst == 1])

  return(
    Ends[between(lubridate::year(Ends), min(year), max(year))]
  )
}

USDaylightSavingsYears <- function(year){
  lubridate::year(USDaylightSavingsBegins(year))
}

HolidayReligions <- function(christian = FALSE, jewish = FALSE, islamic = FALSE){

  Religions <- c(
    Christian = christian,
    Islamic = islamic,
    Jewish = jewish
  )

  return(bind_rows(Religions))
}

x <- HolidayReligions()

HolidayGeos <- function(
  geo_USA = FALSE,
  geo_CAN = FALSE,
  geo_CAN_QC = FALSE,
  geo_CAN_NS = FALSE,
  geo_CAN_NB = FALSE,
  geo_CAN_MB = FALSE,
  geo_CAN_BC = FALSE,
  geo_CAN_PE = FALSE,
  geo_CAN_SK = FALSE,
  geo_CAN_AB = FALSE,
  geo_CAN_NL = FALSE
){
  Geos <-
    c(
      Geo_USA = geo_USA,
      Geo_CAN_QC = geo_CAN_QC || geo_CAN,
      Geo_CAN_NS = geo_CAN_NS || geo_CAN,
      Geo_CAN_NB = geo_CAN_NB || geo_CAN,
      Geo_CAN_MB = geo_CAN_MB || geo_CAN,
      Geo_CAN_BC = geo_CAN_BC || geo_CAN,
      Geo_CAN_PE = geo_CAN_PE || geo_CAN,
      Geo_CAN_SK = geo_CAN_SK || geo_CAN,
      Geo_CAN_AB = geo_CAN_AB || geo_CAN,
      Geo_CAN_NL = geo_CAN_NL || geo_CAN
    )

  return(bind_rows(Geos))
}

CreateHolidayDataFrame <- function(
  holiday,
  dates,
  religions = HolidayReligions(),
  geos = HolidayGeos(geo_USA = TRUE, geo_CAN = TRUE)
){

  # we need to assert religions and geos are proper vectors

  Dates <- as.Date(dates)
  Years <- lubridate::year(Dates)

  data_frame(
    Holiday = holiday,
    Year = Years,
    Date = Dates
  ) %>%
    cross_join(religions) %>%
    cross_join(geos)
}

Holidays <- bind_rows(

  # Secular Holidays

  CreateHolidayDataFrame(
    holiday = "New Years Day",
    dates = as.Date(timeDate::NewYearsDay(year = Years))
  ),

  CreateHolidayDataFrame(
    holiday = "Halloween",
    dates = as.Date(Halloween(year = Years))
  )


#   , data_frame(
#     Holiday = "Halloween",
#     Year = Year,
#     Date = as.Date(Halloween(year = Year)),
#     Religion = NA_character_
#   ) %>%
#     cross_join(data_frame(Country = c("USA", "CAN"), Province = NA_character_))
#
#
#
#
#   # US Holidays
#
#   , data_frame(
#     Holiday = "Inauguration Day",
#     Year = Year,
#     Date = as.Date(timeDate::USInaugurationDay(year = Year)),
#     Religion = NA_character_
#   ) %>%
#     cross_join(data_frame(Country = c("USA", "CAN"), Province = NA_character_))
#
#
#   , data_frame(
#     Holiday = "Columbus Day",
#     Year = Year,
#     Date = as.Date(timeDate::USColumbusDay(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#   , data_frame(
#     Holiday = "President's Day",
#     Year = Year,
#     Date = as.Date(timeDate::USPresidentsDay(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#   , data_frame(
#     Holiday = "Independence Day",
#     Year = Year,
#     Date = as.Date(timeDate::USIndependenceDay(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#   , data_frame(
#     Holiday = "Labor Day",
#     Year = Year,
#     Date = as.Date(timeDate::USLaborDay(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#   , data_frame(
#     Holiday = "Martin Luther Kings Birthday",
#     Year = Year,
#     Date = as.Date(timeDate::USMLKingsBirthday(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#   , data_frame(
#     Holiday = "Election Day",
#     Year = Year,
#     Date = as.Date(timeDate::USElectionDay(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#   , data_frame(
#     Holiday = "Veterans Day",
#     Year = Year,
#     Date = as.Date(timeDate::USVeteransDay(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#   , data_frame(
#     Holiday = "Thanksgiving",
#     Year = Year,
#     Date = as.Date(timeDate::USThanksgivingDay(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#   , data_frame(
#     Holiday = "Daylight Savings Begins",
#     Year = USDaylightSavingsYears(year = Year),
#     Date = as.Date(USDaylightSavingsBegins(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#   , data_frame(
#     Holiday = "Daylight Savings Ends",
#     Year = USDaylightSavingsYears(year = Year),
#     Date = as.Date(USDaylightSavingsEnds(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#
#   , data_frame(
#     Holiday = "Post OSHA Log",
#     Year = USDaylightSavingsYears(year = Year),
#     Date = as.Date(USOshaLogPost(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#
#   ,data_frame(
#     Holiday = "File OSHA Log",
#     Year = Year,
#     Date = as.Date(USOshaLogFile(year = Year)),
#     Religion = NA_character_,
#     Country = "USA",
#     Province = NA
#   )
#   # Christian Holidays
#
#   , data_frame(
#     Holiday = "Good Friday",
#     Year = Year,
#     Date = as.Date(timeDate::GoodFriday(year = Year)),
#     Religion = "Christian"
#   ) %>%
#     cross_join(
#       data_frame(
#         Country = c("USA", "CAN"),
#         Province = NA
#       )
#     )
#
#   , data_frame(
#     Holiday = "Easter",
#     Year = Year,
#     Date = as.Date(timeDate::Easter(year = Year)),
#     Religion = "Christian"
#   ) %>%
#     cross_join(data_frame(Country = c("USA", "CAN")))
#
#   , data_frame(
#     Holiday = "Easter Monday",
#     Year = Year,
#     Date = as.Date(timeDate::EasterMonday(year = Year)),
#     Religion = "Christian"
#   ) %>%
#     cross_join(data_frame(Country = c("USA", "CAN")))
#
#   , data_frame(
#     Holiday = "Christmas",
#     Year = Year,
#     Date = as.Date(timeDate::ChristmasDay(year = Year)),
#     Religion = "Christian"
#   ) %>%
#     cross_join(data_frame(Country = c("USA", "CAN")))
#
#
#   , data_frame(
#     Holiday = "Boxing Day",
#     Year = Year,
#     Date = as.Date(timeDate::BoxingDay(year = Year)),
#     Religion = "Christian"
#   ) %>%
#     cross_join(data_frame(Country = c("USA", "CAN")))
#

)

SecularPublicHolidays <- c("New Years Day", "Christmas")
ChristianPublicHolidays <- c("Christmas", "Easter")
USPublicHolidays <- c("Memorial Day", "Independence Day", "Labor Day", "Thanksgiving")


Holidays %<>%
  mutate(
    PublicHoliday = FALSE
  ) %>%
  mutate(
    PublicHoliday = if_else(Holiday %in% SecularPublicHolidays, TRUE, PublicHoliday)
  ) %>% mutate(
    PublicHoliday = if_else(Holiday %in% ChristianPublicHolidays, TRUE, PublicHoliday)
  ) %>%
  mutate(
    PublicHoliday = if_else(Geo_USA & (Holiday %in% USPublicHolidays), TRUE, PublicHoliday)
  )





write_csv(Holidays, "data-raw/holidays.csv")
devtools::use_data(Holidays, overwrite = TRUE)

