import_fasta <- function() {
  
  
  
  fasta <- Biostrings::readAAStringSet(filepath = "Data\\Human_21042021_uniprot-reviewed yes+taxonomy 9606.fasta")
  
  
  proteome <- sapply(X = names(fasta), FUN = function(x) strsplit_(x, "\\|")[2], USE.NAMES = F)
  
  library(org.Hs.eg.db)
  entrez.genome <- AnnotationDbi::select(x = org.Hs.eg.db, keys = proteome, columns = "ENTREZID", keytype = "UNIPROT")
  
  
  
  
  
  
  
  
  
  
  
}