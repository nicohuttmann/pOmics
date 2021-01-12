add_data <- function(data, name, type, dataset, set.default = T) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Check data type and name
  data.name <- get_data_name(dataset, type, name)



  #Add
  .datasets[[dataset]]





}
