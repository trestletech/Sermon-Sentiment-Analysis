getSentiment <- function (text, key){
  library(RCurl);
  library(RJSONIO);
  
  
  text <- URLencode(text);
  
  #save all the spaces, then get rid of the weird characters that break the API, then convert back the URL-encoded spaces.
  text <- str_replace_all(text, "%20", " ");
  text <- str_replace_all(text, "%\\d\\d", "");
  text <- str_replace_all(text, " ", "%20");
  
  
  if (str_length(text) > 360){
    text <- substr(text, 0, 359);
  }
  
  
  data <- getURL(paste("http://www.viralheat.com/api/sentiment/review.json?text=",text,"&api_key=",key, sep=""))
  
  js <- fromJSON(data, asText=TRUE);
  
  mood <- js$prob;
  
  j <<- js;
  
  
  if (js$mood == "negative"){
    mood <- mood * -1;
  }
  else{
    if (js$mood == "positive"){
      
    }
    else{
      #must be neutral
      mood <- 0;
    }
  }
      
  return(mood);
}


    
        
        