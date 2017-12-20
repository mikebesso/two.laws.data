
#' @export
LoadPackageData <- function(dataset, package){

  envir <- new.env(parent = emptyenv())
  data(list = dataset, package = package, envir = envir)


  Dataset <- get(dataset, envir = envir)

  return(Dataset)

}




#' @export
GenerateRandomPeople <- function(n = 100){

  PostalCodes <- dplyr::sample_n(LoadPackageData("us.postalcodes", "two.laws.data"), n)
  Names <- dplyr::sample_n(LoadPackageData("us.babynames", "two.laws.data"), n)

  People <- bind_cols(PostalCodes, Names)
  return(People)
}


#x <- GenerateRandomPeople()