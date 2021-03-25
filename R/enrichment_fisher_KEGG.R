enrichment_fisher_KEGG <- function() {



  organism_code <- clusterProfiler::search_kegg_organism(get_dataset_attr(which = "species", dataset = dataset),
                                                         by = "scientific_name")[1, 1]
  rm(kegg_species)

  proteins <- res[["genotype"]]
  proteins <- res[["treatment"]]
  proteins <- res[["genotype:treatment"]]
  background <- analysis_list[["results"]][["variables"]]
  #background <- get_all_variables()



  kegg.results <- clusterProfiler::enrichKEGG(gene = proteins,
                                              universe = background,
                                              organism = organism_code,
                                              keyType = "uniprot",
                                              pAdjustMethod = "none",
                                              minGSSize = 10,
                                              pvalueCutoff = 0.05)


  results <- tibble::as_tibble(kegg.results@result)
  results <- results %>%
    dplyr::filter(pvalue < 0.05) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Proteins = paste(strsplit(geneID, split = "/")[[1]], collapse = ";"), .before = geneID) %>%
    dplyr::select(-c(geneID, p.adjust, qvalue)) %>%
    dplyr::mutate(Genes = paste(p2g(strsplit(Proteins, split = ";")[[1]]), collapse = ";"), .before = Proteins)

  View(results)



}
