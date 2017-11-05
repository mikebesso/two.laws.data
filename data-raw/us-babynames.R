# library(two.laws.big.bang)
# library(two.laws.dev)

PackageData <- PackageDataClass$new("us-babynames")

PackageData$DownloadAndExtractZip("https://www.ssa.gov/oact/babynames/names.zip")

# From: http://www.ssa.gov/oact/babynames/limits.html
all <- dir(PackageData$CacheFolder, "\\.txt$", full.names = TRUE)

all %<>% sort()

ReadBabyNameFile <- function(path){
  BabyNames <- readr::read_csv(path, col_names = FALSE, col_types = list(X2 = col_character()))
  names(BabyNames) <- c("Name", "Sex", "Count")
  BabyNames$Year = as.numeric(gsub("[^0-9]", "", basename(path)))

  BabyNames %<>%
    tbl_df() %>%
    select(Year, Sex, Name, Count) %>%
    arrange(Sex, desc(Count))

  return(BabyNames)
}

us.babynames <- lapply(all, ReadBabyNameFile)

devtools::use_data(us.babynames, compress = "xz", overwrite = T)