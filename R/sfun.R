
#' Main function
#'
#' @param mydata data.frame
#' @param surdata data.frame
#' @return data.frame with 3 columns; from, to and component
#' @export
#'
sfun = function(mydata, alpha, surdata=NULL, scoreFn = "bic", pheno = FALSE, alpha1 = 0.01, alpha2 = 0.01, pp = NULL){
  multBNs = FALSE
  genes = NULL
  pOrd = NULL
  #print(scoreFn)
  cnames = colnames(mydata)
  #print(cnames)
  n.var = ncol(mydata)
  surv = 0
  # survival processing
  if(!is.null(surdata)){
    #print("suuuuuuuu")
    mydata=cbind(mydata,surdata)
    n.var = ncol(mydata)
    n.var = n.var -1 # last col survival time
    surv = 1
    if(is.null(pp)){
      pp = mypp_surv(mydata, genes, alpha1, alpha2)
      }
    mydata1 = mydata[genes]
    mydata=cbind(mydata1,surdata)
    cnames = colnames(mydata)
    feasS = NULL
    i=1
    for (v in pp) {
      if (!is.null(v)){
        feasS = c(feasS,i)
        for (k in pp[i]) {
          feasS = c(feasS,k)
        }
      }
      i=i+1
    }
    feasS= unique(feasS)
    #print(feasS)
    feasS= feasS[order(feasS)]
    #print(feasS)
    #print("feasS#====")
    #print(length(feasS))
    pp1 = vector('list', length(feasS))
    i=1
    for (g in pp) {
      if(!is.null(g)){
        pp1[[match(i,feasS)]] = match(pp[[i]],feasS)
      }

      i=i+1
    }
    i=1
    for (g in pp1) {
      if(!is.null(g)){
        pp1[[i]] = unique(pp1[[i]])
      }

      i=i+1
    }
    pp = pp1
  }
  # survival processing end

  # non-survival processing
if(is.null(pp)){
  #print("000000000")
  if (pheno == TRUE){
    pp = mypp1(mydata[-(n.var+1)], alpha1,alpha2, n.var, n.var) # phenotype based
  }
  else{
    if(is.null(alpha)){
      alpha = 0.85
      }
    pp = mypp(mydata, alpha, n.var)
  }
  #print("111111")
  feasS = NULL
  i=1
  for (v in pp) {
    #print(v)
    if (!is.null(v)){
      feasS = c(feasS,i)
      for (k in pp[i]) {
        feasS = c(feasS,k)
          }
        }
        i=i+1
      }
      feasS= unique(feasS)
      #print(feasS)
      feasS= feasS[order(feasS)]
      #print("feasS#====")
      #print(length(feasS))
      #print(feasS)

      pp1 = vector('list', length(feasS))
      i=1
      for (g in pp) {
        if(!is.null(g)){
          pp1[[match(i,feasS)]] = match(pp[[i]],feasS)
        }

        i=i+1
      }
      i=1
      for (g in pp1) {
        if(!is.null(g)){
          pp1[[i]] = unique(pp1[[i]])
        }

        i=i+1
      }
      pp = pp1
  }# non-survival processing end

  #down keeping only feasS data
  #print(feasS)
  mydata = mydata[feasS]
  cnames1 = colnames(mydata)
  n.var = ncol(mydata)
  if(!is.null(surdata)){
    mydata=cbind(mydata,surdata[[2]])
  }

  ms = mscores(1:n.var, mydata, surv, scoreFn) # node scores, no parents
  #print(ms)

  #print("after pp")

  po = pofun(pp) # possible offspring
  #print("after po")

  #save(pp,po,file = "/Users/nandshar/Josh/GOBnilp/pygobnilp-1.0/TestDS/current_0.RData")

  max_parents = 2
  if(!is.null(pOrd))
    {
    pps = pp.sets(pp)
    #pps = pp.sets1(pp, pOrd) # all sets of possible parents
  }
  else{
    pps = pp.sets(pp) # all sets of possible parents
  }
  #print("after pps")
  #save(pp,po,pps,file = "/Users/nandshar/Josh/GOBnilp/pygobnilp-1.0/TestDS/current_0.RData")

  if(!is.null(pOrd))
  {
    ppss = pp.sets.s(mydata, pps,surv,scoreFn)
    #ppss = pp.sets.s.pOrd(mydata, pps,surv,scoreFn, pOrd)
  }
  else{
    ppss = pp.sets.s(mydata, pps,surv,scoreFn) # scores for all sets of possible parents for each node
  }

  #print("after ppss")
  bps = pp.sets.bs(pps, ppss, ms, max_parents, surv) # BEST parent sets and scores for all sets of possible parents for each node
  #print("after bps")
  #save(ms,pp,po,pps,ppss,bps,file = "/Users/nandshar/Josh/GOBnilp/pygobnilp-1.0/TestDS/current_1.RData")
  if(!is.null(pOrd))
  {
    bsinks = bestSinksPartialOrds(pp, ms, po, pps, ppss, bps, mydata, surv,max_parents, pOrd)
  }
  else{
    bsinks = bestSinks(pp, ms, po, pps, ppss, bps, mydata, surv) # best sinks for all possible connected components
    #bsinks = bestSinks1(pp, ms, po, pps, ppss, bps, mydata, surv)
  }
  #print(bsinks,quote = TRUE, row.names = FALSE)
  #print("after bsinks")

  if(nrow(bsinks)>0){
    bnets = bestnet(bsinks, n.var) # ordered best sinks for labeled connected components
    #print("after bnets")
    #print(bnets,quote = TRUE, row.names = FALSE)
    #save(ms,pp,po,pps,ppss,bps,bsinks,bnets,file = "/Users/nandshar/Josh/GOBnilp/pygobnilp-1.0/TestDS/current_1.RData")
    # multiple best nets
    if(multBNs == TRUE){
      allNets = findAllBestNets(bsinks, n.var)

      multBestNets = vector('list', length(allNets))
      mylinks = NULL
      mylinks1 = NULL
      mylinks11 = NULL
      for(i in 1:length(allNets)){
        mylinks = sink2net(allNets[[i]], pp, pps, bps)
        # mylinks has node numbers in feasS data;
        # sources, sinks, mylinks1 has correct node numbers according to feasS;
        # sources1, sinks1, mylinks11 has node names.
        sources = list()
        sources1 = list() # names
        sources =feasS[mylinks[,"node.source"]]
        sources1 =cnames[sources]
        #print(sources)
        sinks = list()
        sinks1 = list() #names
        sinks =feasS[mylinks[,"node.sink"]]
        sinks1 =cnames[sinks]
        #print(sinks)
        mylinks1 = mylinks
        mylinks11 = mylinks
        mylinks1[,"node.source"] = sources

        mylinks1[,"node.sink"] = sinks
        mylinks11[,"node.source"] = sources1
        mylinks11[,"node.sink"] = sinks1
        #print(mylinks11,quote = TRUE, row.names = FALSE)
        multBestNets[[i]]= mylinks11
        #save(feasS,mydata,ms,pp,po,pps,ppss,bps,bsinks,allNets,multBestNets,file = "/Users/nandshar/Josh/GOBnilp/pygobnilp-1.0/TestDS/current_2.RData")
      }

      }
    else {
      # mylinks has node numbers in feasS data;
      # sources, sinks, mylinks1 has correct node numbers according to feasS;
      # sources1, sinks1, mylinks11 has node names.
      mylinks = sink2net(bnets, pp, pps, bps)
        sources = list()
        sources1 = list() # names
        sources =feasS[mylinks[,"node.source"]]
        sources1 =cnames[sources]
        #print(sources)
        sinks = list()
        sinks1 = list() #names
        sinks =feasS[mylinks[,"node.sink"]]
        sinks1 =cnames[sinks]
        #print(sinks)
        mylinks1 = mylinks
        mylinks11 = mylinks
        mylinks1[,"node.source"] = sources

        mylinks1[,"node.sink"] = sinks
        mylinks11[,"node.source"] = sources1
        mylinks11[,"node.sink"] = sinks1

        #print(mylinks1,quote = TRUE, row.names = FALSE)
        #print(mylinks11,quote = TRUE, row.names = FALSE)
    }

  }
  if(multBNs == TRUE){
      return(multBestNets)
    }else
      return(mylinks11) # links1 has numbers, mylinks11 has node names
} # end sfun

