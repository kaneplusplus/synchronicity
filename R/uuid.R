# UUID

#' @title Create a universal unique identifier.
#' @description This function creates an identifier that will be (with high 
#' probability) unique on a single machine or group of machines.
#' @return A unique string.
#' @examples
#' print(uuid())
#' @export
uuid=function()
{
  uuid_len = 24
  if (Sys.info()['sysname'] == "Darwin") uuid_len = 15
  paste(sample(c(letters, LETTERS), uuid_len, replace=TRUE), collapse="")
}

