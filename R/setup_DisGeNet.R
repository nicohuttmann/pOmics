#' Registers API key to use disgenet2r (https://www.disgenet.org/signup/)
#'
#' @param email email for DisGeNet account
#' @param password password for DisGeNet account
#'
#' @return
#' @export
#'
#'
setup_DisGenet <- function(email = "nhutt069@uottawa.ca",
                           password = "makey0ur0wnacc0unt") {


  disgenet_api_key <- disgenet2r::get_disgenet_api_key(
    email = email,
    password = password)

  # Check
  if (is.null(disgenet_api_key)) {
    message("Email and password did not match an existing account.
            Go to https://www.disgenet.org/signup/ and sign up.")
    return(invisible(FALSE))
  }

  # Register api key
  Sys.setenv(DISGENET_API_KEY = disgenet_api_key)

  if (email == "nhutt069@uottawa.ca") {
    message("API key successfully registered. Please make your own account.")
  } else {
    message("API key successfully registered.")
  }


  return(invisible(TRUE))

}
