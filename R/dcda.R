
dcda <- function(fastas, 
                 pfamA, 
                 cut = 'ga', 
                 distMethod = 'bray', 
                 cpus = 1L){
  #Eval - err
  
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
  mat <- parallel::mclapply(fastas, function(x){
    
    tmp <- tempfile()
    hmmres <- hmmSearch(x, pfamA, cut = cut, oty = 'domtblout', n_threads = 0)
    dtbl <- readDomtblout(domtblout = hmmres)
    tab <- table(factor(dtbl$PfamID, levels = ids))
    tab
    
  }, mc.preschedule = FALSE, mc.cores = cpus)
  
  mat <- do.call(rbind, mat)
  
  #Compute dist
  
  
  #Return
  
}