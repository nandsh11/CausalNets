# function to loop through offsprings to find best sinks
bestSinks1 = function(pp, ms, po, pps, ppss, bps, mydata, surv){
  print("inside bestSinks")
  m = ncol(mydata) # number of nodes
  if(surv == 1){
    m = m-1
  }
  nms = c("windx", "k", "sink", "wscore") #subset
  sinks.tmp = as.data.frame(matrix(NA, nrow=0, ncol=length(nms)))
  rownames(sinks.tmp) <- NULL
  names(sinks.tmp) = nms

  # best sinks and scores for subnetworks of one node, which is the node itself and its score
  for(s in 1:m){
    sinks.tmp[s, "windx"] = subsetr(m, s)
    sinks.tmp[s, "k"] = 1
    sinks.tmp[s, "sink"] = s
    sinks.tmp[s, "wscore"] = ms[s]
  }

  mysinks = sinks.tmp
  mysinks = round(mysinks,4)
  print("mysinks111111111")
  print(mysinks,quote = TRUE, row.names = FALSE)
  bsinks = sinks.tmp[0, ] # names row

  # best sinks and scores for subnetworks of size 2:m
  for(q in 2:m){
    #print("q")
    #print(q)
    sinks.tmp1 = list()
    wscore <- windx <- k <- sink <- numeric(m*m)
    index <- 1
    for(j in 1:nrow(sinks.tmp)) {
      #print("j")
      #print(j)

      w = subsetur(m, sinks.tmp[j, "windx"])
      print("w")
      print(w)
      w.networkscore = sinks.tmp[j, "wscore"]
      w1sinks = wsink.scores(w, w.networkscore, pp, po, pps, bps, m)
      #print("w1sinks")
      #print(w1sinks)
      index_subset <- c(index:(index-1 + length(w1sinks$wscore)))
      print("index_subset")
      print(index_subset)
      wscore[index_subset] <- w1sinks$wscore
      windx[index_subset] <- w1sinks$windx
      k[index_subset] <- w1sinks$k
      sink[index_subset] <- w1sinks$sink
      index <- index + length(index_subset)
    }
    sinks.tmp1  <- data.frame(wscore = wscore[seq_len(index-1)],
                              windx = windx[seq_len(index-1)],
                              k = k[seq_len(index-1)],
                              sink = sink[seq_len(index-1)])
    print("sinks.tmp1")
    sinks.tmp1 = round(sinks.tmp1,4)
    print(sinks.tmp1,quote = TRUE, row.names = FALSE)
    # sinks.tmp1 has all sinks at level q
    # now keep only the best sinks for each windex
    # and add to bsinks
    # for each subset w, find the best sinks
    # keep the row/rows with max score
    myws = unique( sinks.tmp1$windx )
    for(wind in 1:length(myws)){
      print("wind")
      print(wind)
      myw = myws[ wind ]
      tmp = sinks.tmp1[ is.element( sinks.tmp1$windx, myw ), ]
      #print("tmp")
      #print(tmp)
      ks = unique( tmp$wscore )
      M = max(ks)
      tmp1 = tmp[ round(as.double(tmp$wscore), digits=4) == round(as.double(M), digits=4), ]
      print("tmp1")
      #print(tmp1)
      bsinks = rbind( bsinks, tmp1 )
    }
    bsinks = round(bsinks,4)
    print("bsinks")
    #print(bsinks,quote = TRUE, row.names = FALSE)
    bsinks = unique(bsinks)
    # add best sinks rows to sinks.tmp before end q for loop
    sinks.tmp = bsinks
    print(bsinks,quote = TRUE, row.names = FALSE)
    # print("sinks.tmp")
    # print(sinks.tmp,quote = TRUE, row.names = FALSE)

  } # end q for loop

  print(bsinks,quote = TRUE, row.names = FALSE)
  bsinks11 = bsinks
  wsubCol = NULL
  if(nrow(bsinks)>0){
    for(i in 1:nrow(bsinks)){
      if(is.na(bsinks[i,"windx"])) wsubset = NA
      else wsubset <- paste0(subsetur(m,bsinks[i,"windx"]), collapse = ",")
      wsubCol = c(wsubCol,wsubset)
    }

  }

  bsinks["subset"] = wsubCol
  #print(bsinks,quote = TRUE, row.names = FALSE)

  return(bsinks)


} # end bestSinks
