# UUID

#' @export
uuid=function()
{
  return(.Call('boost_create_uuid', PACKAGE="synchronicity"))
}

