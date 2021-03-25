enrichment_fisher_KEGG <- function() {
  
  
  
  organism_code <- clusterProfiler::search_kegg_organism(get_dataset_attr(which = "species", dataset = dataset),
                                                         by = "scientific_name")[1, 1]
  rm(kegg_species)
  
  proteins <- intersect(proteins.HJV$variables, proteins.DEN$variables)
  proteins <- df.hjv$variables
  proteins <- which_names(pull(hjv.diff, var = `CTRL:HJV-CTRL:WT`, name = variables) > 0.6)
  background <- proteins.anova$variables
  background <- get_all_variables()
  
  
  
  kegg.results <- clusterProfiler::enrichKEGG(gene = proteins,
                                              universe = background,
                                              organism = organism_code,
                                              keyType = "uniprot",
                                              pAdjustMethod = "none",
                                              minGSSize = 10,
                                              pvalueCutoff = 0.05,
                                              qvalueCutoff = 0.2)
  View(kegg.results@result)
  
  
  clusterProfiler::simplify(x = kegg.results)
  
  
  genes <- clusterProfiler::bitr(geneID = proteins, fromType = "ACCNUM", toType = "SYMBOL", OrgDb = org.Mm.eg.db, drop = T)
  
  
  
  start <- Sys.time()
  a <- clusterProfiler::groupGO(gene = proteins, OrgDb = org.Mm.eg.db, keyType = "ACCNUM", ont = "CC", readable = T)
  print(Sys.time() - Sys.time())
  
  
  
  
  results <- clusterProfiler::enrichKEGG(gene = ,
                                         organism = , 
                                         keyType = , 
                                         pvalueCutoff = , 
                                         pAdjustMethod = , 
                                         universe = , 
                                         minGSSize = , 
                                         maxGSSize = , 
                                         qvalueCutoff = , 
                                         use_internal_data = )
  
}
