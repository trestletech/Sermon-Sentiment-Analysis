
#' Plot a line showing the trend of the sentiments of a sermon
#'
#' Plots the points of the sentiments of the sentences given and then fits a smoothing spline on top to show the trend
#'
#' @param sermon the sermon to analyze -- should have field: author, title, and serm
#' @author Jeff Allen \email{jeff.allen@@trestletechnology.net}
plotSentenceTrend <- function(sermon){
  sent <- sermon$sent;
  sermAuthor <- sermon$author;
  sermTitle <- sermon$title;
  serm <- sermon$serm;
  
  cols <- rep(1:length(sent), sapply(sent,length));
  sents <- unlist(sent);
  
    plot(1:length(sents), sents, col=cols, pch=cols, main=paste(sermAuthor,"-", sermTitle), ylab="Sentiment", xlab="Sentences Spoken");
     lines(smooth.spline(1:length(unlist(sent)), unlist(sapply(sent, as.numeric)), df=3), col=1)
 
}

#' Plots a word cloud given the word table
#' 
#' @author Jeff Allen \email{jeff.allen@@trestletechnology.net}
myCloud <- function (w, col, yspace = 1.3, xspace = 0.1, minh = 0, add=FALSE, xmin = 0, xmax = 0.98, ystart=0.95, ...) 
{
    if (missing(col)) 
        col <- "#000000"
    omar <- par("mar")
    par(mar = c(0, 0, 0, 0))
    if (!add){
      plot(xmin:xmax, 0:ystart, type = "n", axes = FALSE)
    }
    x = xmin
    y = ystart
    xch = minh
    cm = 3/max(w)
    . <- lapply(1:length(w), function(i) {
        cex = w[i] * cm
        ctw = strwidth(names(w[i]), cex = cex)
        cth = strheight(names(w[i]), cex = cex)
        if (cth > xch) 
            xch <<- cth
        if (x + ctw > xmax) {
            x <<- xmin
            y <<- y - (yspace * xch)
            xch <<- minh
        }
        text(x, y, names(w[i]), cex = cex, adj = c(0, 0.5), col = col[i])
        x <<- x + ctw + xspace
    })
    par(mar=omar)
    invisible(TRUE)
}

#' Plot the sentiment trend with the words that are most positively sentimented in the top and lowly sentimented on the bottom
#' @author Jeff Allen \email{jeff.allen@@trestletechnology.net}
plotHighLow <- function(sermon, sentenceLevel = TRUE){
    
    sent <- sermon$sent;
  sermAuthor <- sermon$author;
  sermTitle <- sermon$title;
  serm <- sermon$serm;
 
    
    omar <- par("mar")
    par(mar = c(2, 2, 2, 2))
  
    plot(1:length(sent), sapply(sapply(sent, as.numeric), median), main=paste(sermAuthor,"-", sermTitle), xlab="Paragraphs Spoken", ylab="Sentiment", pch=".")
    #text(1:length(sent), sapply(sapply(sent, as.numeric), median), 1:length(sent))
    lines(smooth.spline(1:length(sent), sapply(sapply(sent, as.numeric), median), df=3), col=3)
 
    abline(h=0);
  
  
    library(snippets)
library(tm)

  if (!sentenceLevel){
    #paragraph level
    positivePars <- sapply(sapply(sent, as.numeric), median) > 0;
    negativePars <- sapply(sapply(sent, as.numeric), median) < 0;
  
    words <- tolower(unlist(lapply(serm[positivePars], function(x) strsplit(x, " "))))
    pt <- getWordTable(words, maxWords = 45);
  
    words <- tolower(unlist(lapply(serm[negativePars], function(x) strsplit(x, " "))))
    nt <- getWordTable(words, maxWords = 45);
  
  } else {
      
    #sentence level
    positiveSents <- sapply(unlist(sent), as.numeric) > 0;
    negativeSents <- sapply(unlist(sent), as.numeric) < 0;
  
    words <- tolower(unlist(lapply(unlist(serm)[positiveSents], function(x) strsplit(x, " "))))
    pt <- getWordTable(words, maxWords = 45);
  
  
    words <- tolower(unlist(lapply(unlist(serm)[negativeSents], function(x) strsplit(x, " "))))
    nt <- getWordTable(words, maxWords = 45);
   
    
    par(mar=omar);
    
  }
    
    
  #require exclusivity
  ptnew <- pt[!names(pt) %in% names(nt)]
  ntnew <- nt[!names(nt) %in% names(pt)]

  myCloud(ptnew, col = col.br(ptnew, fit=TRUE), add=TRUE, xmin=1,xmax=length(sent)+.5,ystart=.80)
  myCloud(ntnew, col = col.br(ntnew, fit=TRUE), add=TRUE, xmin=1,xmax=length(sent)+.5,ystart=-.25)
     
  return(list(neg=ntnew,pos=ptnew));
    
}

#' Identify the unique words in two dictionaries
#' @author Jeff Allen \email{jeff.allen@@trestletechnology.net}
getUniqueWords <- function (dictA, dictB, cutoff){  
  inCommon <- dictA[which(names(dictA)[1:cutoff] %in% names(dictB)[1:cutoff])]
  aUnique <- dictA[which(!names(dictA)[1:cutoff] %in% names(dictB)[1:cutoff])]
  bUnique <- dictB[which(!names(dictB)[1:cutoff] %in% names(dictA)[1:cutoff])]
  return(list(common=inCommon,a=aUnique,b=bUnique));
}

  
#' Given sentiments of multiple sermons, show their average trends
#' @author Jeff Allen \email{jeff.allen@@trestletechnology.net}
plotAuthorsAvg <- function(authorsSent, title=""){
  mattSent <- authorsSent;    

  mattSent <- lapply(mattSent, function(x){lapply(x, as.numeric)});
      
  mattPar <- lapply(mattSent, function(x){sapply(x, mean)});
  mattPar <- lapply(mattPar,function(x){n <- x > 0; n <- n *2; n <- n-1; return((n) * (abs(x)^2))})
      
  plot(1,1, xlim=c(0,1),ylim=c(-1,1),type="n", xlab="Position in Sermon", ylab="Sentiment", main=title);
  sermXs <- array();
  sermYs <- array();
  for (i in 1:length(mattPar)){
    thisSerm <- mattPar[[i]];
    points(x=seq(0,1,length.out=length(thisSerm)), y=thisSerm, pch=".")
    sermXs <- c(sermXs, seq(0,1,length.out=length(thisSerm)));
    sermYs <- c(sermYs,thisSerm);
    lines(smooth.spline(x=seq(0,1,length.out=length(thisSerm)), y=thisSerm, df=3), col="#DDDDDD")
  }
    
  lines(smooth.spline(x=sermXs, y=sermYs, df=3), lwd=2)
}
 