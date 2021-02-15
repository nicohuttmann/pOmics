compare_two_networks <- function() {





  results <- get_data(variables = clean, observations = str_detect(groups, i), type = "Identification") %>%
    eval_data_grouped_var(groups = groups,
                          expr = mean(x != ""),
                          view = F) %>%
    classify_protein_identification_2()









}
