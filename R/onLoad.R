# .onAttach function for startup message
.onAttach <- function(lib, pkg) {
  msg <- paste0(
    "This is KrigR (version ", packageVersion("KrigR"),
    "). Note that as of version 0.5.0, considerable changes from the development path have been merged with the main branch leading to the deprecation of some older and outdated functionality. ",
    "If you have used a version of KrigR prior to 0.5.0, we strongly recommend you re-familiarise yourself with the complete suite of KrigR. ",
    "This message will keep showing until KrigR version 1.0.0 is achieved."
  )
  packageStartupMessage(msg)
}

# Roxygen import declaration for usethis
#' @importFrom usethis use_citation
NULL
