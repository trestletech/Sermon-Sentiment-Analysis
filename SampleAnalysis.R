
  #myCloud(wt, col = col.br(wt, fit=TRUE), add=TRUE, xmin=12,xmax=23,ymin=.9)
  #myCloud(wt, col = col.br(wt, fit=TRUE), add=TRUE, xmin=0,xmax=17,ymin=-.4)
  

    
    #calculate global positive and negatives        
w5 <- plotHighLow(sentiments[[6]])
w6 <- plotHighLow(sentiments[[6]])
w5 <- plotHighLow(sentiments[[5]])
w4 <- plotHighLow(sentiments[[4]])
w3 <- plotHighLow(sentiments[[3]])
w2 <- plotHighLow(sentiments[[2]])
w1 <- plotHighLow(sentiments[[1]])
    
 na <- c(w1$neg, w2$neg, w3$neg, w4$neg, w5$neg, w6$neg)
 pa <- c(w1$pos, w2$pos, w3$pos, w4$pos, w5$pos, w6$pos)
 
 
 na <- table(rep(names(na), na))
 pa <- table(rep(names(pa), pa))
    
  ptnew <- pa[!names(pa) %in% names(na)]
  ntnew <- na[!names(na) %in% names(pa)]
    
  par(mfrow=c(1,2))
  cloud(ntnew)
  cloud(ptnew)
    
    
  #Look at overall sentiment:
  sents <- list(); 
  authors <- array();
  sentAvg <- array();
  for (i in 1:length(sentiments)){
    m <- mean(sapply(sapply(sentiments[[i]]$sent, as.numeric), mean)) 
    if (!is.null(sents[[sentiments[[i]]$author]])){
      sents[[sentiments[[i]]$author]] <- c(sents[[sentiments[[i]]$author]], m);
    }
    else{
      sents[[sentiments[[i]]$author]] <- m;
    }
    authors[i] <- sentiments[[i]]$author;
    sentAvg[i] <- m;
  }
  
  authorAvg <- data.frame(authors, sentAvg)
  authorAvg[,2] <- as.numeric(authorAvg[,2]);
    
    
  
      
  t.test(sentAvg ~ authors, data=authorAvg)
    
  boxplot(sents, main="Average Sentiment Comparison")
    
  mattSent <- lapply(sentiments[which (sapply(sentiments, "[[", "author") == "Matt Chandler")], "[[", "sent")
  markSent <- lapply(sentiments[which (sapply(sentiments, "[[", "author") == "Mark Driscoll")], "[[", "sent")
  


mattDates <- as.Date(unlist(lapply(sentiments[which (sapply(sentiments, "[[", "author") == "Matt Chandler")], "[[", "date")), format="%m/%d/%Y")
mattAvg <- sapply(sapply(sapply(sentiments[which (sapply(sentiments, "[[", "author") == "Matt Chandler")], "[[", "sent"), function(x){y <- sapply(x,as.numeric); y<- sapply(y, mean); return(y);}), mean)

plot(mattDates, mattAvg);
abline(lm(mattAvg ~ mattDates), col=2);








#dictionary analysis

  mattWords <- unlist(lapply(sentiments[which (sapply(sentiments, "[[", "author") == "Matt Chandler")], "[[", "serm"))
  markWords <- unlist(lapply(sentiments[which (sapply(sentiments, "[[", "author") == "Mark Driscoll")], "[[", "serm"))

  mattWords <- tolower(unlist(strsplit(mattWords, " ")))
  markWords <- tolower(unlist(strsplit(markWords, " ")))

  mattDict <- sort(getWordTable(mattWords), decreasing=TRUE);
  markDict <- sort(getWordTable(markWords), decreasing=TRUE);
  mattCount <- sum(mattDict);
  markCount <- sum(markDict);

  cutoff <- 20;

  inCommonMatt <- mattDict[which(names(mattDict)[1:cutoff] %in% names(markDict)[1:cutoff])]/mattCount
  inCommonMark <- markDict[which(names(markDict)[1:cutoff] %in% names(mattDict)[1:cutoff])]/markCount
  inCommon <- inCommonMatt[order(names(inCommonMatt))] + inCommonMark[order(names(inCommonMark))];
  inCommon <- inCommon/2;
  inCommon[order(inCommon,decreasing=TRUE)]
  mattUnique <- mattDict[which(!names(mattDict)[1:cutoff] %in% names(markDict)[1:cutoff])]/mattCount
  markUnique <- markDict[which(!names(markDict)[1:cutoff] %in% names(mattDict)[1:cutoff])]/markCount
