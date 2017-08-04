#' @name dcda
#' @title Domain Content Dissimilarity Analysis
#' @description Compute Pfam-A domain content dissimilarity between two or more
#' proteomes. 
#' @param fastas A \code{character} vector giving the file names of the amino 
#' acid fasta files.
#' @param pfamA \code{character} The path to the Pfam-A.hmm file
#' @param cut Parameter controlling model-specific thresholding. Can be ethier
#' \code{"ga"} or \code{"tc"}. See HMMER 3.1b2 manual for more information.
#' @param distMethod Dissimilarity index, partial match to "manhattan", 
#' "euclidean", "canberra", "bray" (DEFAULT), "kulczynski", "jaccard", "gower", 
#' "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", 
#' "cao" or "mahalanobis". See \link[vegan]{vegdist} for more information.
#' @param cpus \code{integer} The number of cpus to use.
#' @return A \code{dist} object.
#' @importFrom vegan vegdist
#' @export
dcda <- function(fastas, 
                 pfamA, 
                 cut = 'ga', 
                 distMethod = 'bray', 
                 cpus = 1L){
  #Eval - err
  # ...
  
  #Get pfam-A ids
  cat('Retrieving information from Pfam-A.hmm.. ')
  stats <- hmmStat(hmmfile = pfamA)
  ids <- getIdsFromStats(stats = stats)
  file.remove(stats)
  cat('DONE!\n')
  
  #Press Pfam if not yet.
  idx <- paste0(pfamA, c('.h3f', '.h3i', '.h3m', '.h3p'))
  if (any(!file.exists(idx))){
    cat('Pfam-A.hmm is not indexed. Pressing Pfam-A.hmm.. ')
    hmmPress(pfamA)
    cat('DONE!\n')
  }
  
  #Hmmsearch
  cat('Searching.. ')
  mat <- parallel::mclapply(fastas, function(x){
    
    tmp <- tempfile()
    hmmres <- hmmSearch(x, pfamA, cut = cut, oty = 'domtblout', n_threads = 0)
    dtbl <- readDomtblout(domtblout = hmmres)
    file.remove(tmp)
    tab <- table(factor(dtbl$PfamID, levels = ids))
    tab
    
  }, mc.preschedule = FALSE, mc.cores = cpus)
  cat(' DONE!\n')
  
  #Bind and reduce
  mat <- do.call(rbind, mat)
  rownames(mat) <- sapply(strsplit(fastas, '/'), function(x){ rev(x)[1]})
  mat <- mat[, -which(colSums(mat)==0)]
  
  #Compute dist
  cat('Computing distance/dissimilarity.. ')
  d <- vegan::vegdist(mat, method = distMethod)
  cat('DONE!\n')
  
  #Return
  return(d)
}

