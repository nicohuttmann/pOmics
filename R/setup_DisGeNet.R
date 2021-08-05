setup_DisGenet <- function(email, password) {

  email <- "nhutt069@uottawa.ca"
  password <- "chemistry2018nh."


  disgenet_api_key <- disgenet2r::get_disgenet_api_key(
    email = "emadil",
    password = password)

  # Check
  if (is.null(disgenet_api_key)) {
    message(
"Email and password did not match an existing account.
Go to https://www.disgenet.org/signup/ and sign up.")
    return(invisible(FALSE))
  }

  # Register api key
  Sys.setenv(DISGENET_API_KEY= disgenet_api_key)


}
