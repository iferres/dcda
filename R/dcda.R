
dcda <- function(fastas, 
                 pfamA, 
                 cut = 'ga', 
                 distMethod = 'bray', 
                 cpus = 1L){
  #Eval - err
  
  
  
  #Press Pfam if not yet.
  idx <- paste0(pfamA, c('.h3f', '.h3i', '.h3m', '.h3p'))
  if (any(!file.exists(idx))){
    cat('Pfam-A.hmm is not indexed. Pressing Pfam-A.hmm.. ')
    hmmPress(pfamA)
    cat('DONE!\n')
  }
  
  #Hmmsearch
  res <- parallel::mclapply(fastas, function(x){
    tmp <- tempfile()
    hmmres <- hmmSearch(x, pfamA, cut = cut, oty = 'domtblout', n_threads = 0)
    dtbl <- readDomtblout(domtblout = hmmres)
    
  })
  
  #Read domtblout
  
  #Process domtblout
  
  #Compute dist
  
  #Return
  
}