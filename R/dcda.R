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
#' @return A \code{list} object. The first element is the abundance matrix and
#' the second is the \code{dist} matrix.
#' @importFrom vegan vegdist
#' @export
dcda <- function(fastas, 
                 pfamA, 
                 cut = 'ga', 
                 distMethod = 'bray', 
                 cpus = 1L){
  
  #Eval - err
  if (missing(fastas)){
    stop('No fasta files provided.')
  }
  
  if(length(fastas)<2){
    stop('At least 2 fasta files must be provided.')
  }
  
  if (missing(pfamA)){
    stop('Pfam-A.hmm file path is not provided.')
  }
  
  # if (Sys.which("hmmsearch")==""){
  #   stop("\n\tHMMER (v.3) is not installed. (Couldn't find 'hmmsearch' in $PATH)
  #        \nPlease install it before re-running dcda().\n\n")
  # }
  
  #find binaries
  hmmstat <- system.file('hmmer3.1b2_binaries/hmmstat',
                         package = 'DCDA')
  hmmpress <- system.file('hmmer3.1b2_binaries/hmmpress',
                          package = 'DCDA')
  hmmsearch <- system.file('hmmer3.1b2_binaries/hmmsearch',
                           package = 'DCDA')
  
  #Get pfam-A ids
  cat('Retrieving information from Pfam-A.hmm.. ')
  stats <- hmmStat(bin = hmmstat, hmmfile = pfamA)
  ids <- getIdsFromStats(stats = stats)
  file.remove(stats)
  cat('DONE!\n')
  
  #Press Pfam if not yet.
  idx <- paste0(pfamA, c('.h3f', '.h3i', '.h3m', '.h3p'))
  if (any(!file.exists(idx))){
    cat('Pfam-A.hmm is not indexed. Pressing Pfam-A.hmm.. ')
    hmmPress(bin = hmmpress, model = pfamA)
    cat('DONE!\n')
  }
  
  #Hmmsearch
  cat('Searching.. ')
  mat <- parallel::mclapply(fastas, function(x){
    
    # tmp <- tempfile()
    hmmres <- hmmSearch(bin = hmmsearch, 
                        fasta = x, 
                        hmm = pfamA, 
                        cut = cut, 
                        oty = 'domtblout', 
                        n_threads = 0)
    dtbl <- readDomtblout(domtblout = hmmres)
    file.remove(hmmres)
    tab <- table(factor(dtbl$PfamID, levels = ids))
    tab
    
  }, mc.preschedule = FALSE, mc.cores = cpus)
  cat(' DONE!\n')
  
  #Bind and reduce
  mat <- do.call(rbind, mat)
  rownames(mat) <- sapply(strsplit(fastas, '/'), function(x){ rev(x)[1] })
  colnames(mat) <- ids
  mat <- mat[, -which(colSums(mat)==0)]
  
  #Compute dist
  cat('Computing distance/dissimilarity.. ')
  d <- vegan::vegdist(mat, method = distMethod)
  cat('DONE!\n')
  
  out <- list(mat, d)
  
  #Return
  return(out)
}

