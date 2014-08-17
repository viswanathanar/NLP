options( java.parameters = "-Xmx4g" ) ##to address Java heap space error	
###############################################################
## Function 1 : Begin
## Function to get the date from the given text
## Input: It expects a string and entity annotator with a trained model as input.
## Output: Returns only the date from the text
###############################################################
getEntity <- function(x,entity_annotator)
{
  options(warn=-1)
  s <- as.String(x)
  ## Need sentence and word token annotations.
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
  ## Entity recognition for persons.
  #entity_annotator <- Maxent_Entity_Annotator()
  
  df <- data.frame(annotate(s, entity_annotator, a2))
  if(nrow(df[df$type == 'entity',]) >= 1)
  {
    entity <- s[entity_annotator(s, a2)]
  }
  else
  {
    entity <- 0
  }
  ## Directly:
  #entity_annotator(s, a2)
  ## And slice ...
  return(entity)
}

##Usage : 
#entity_annotator <- Maxent_Entity_Annotator(language = "en", kind = "date", probs = FALSE, model = "S:\\Komma\\Models\\apache-opennlp-1.5.3\\bin\\en-ner-date.bin")
#entity_annotator <- Maxent_Entity_Annotator(language = "en", kind = "date", probs = FALSE, model = NULL)
## getEntity('withdraw money from ATM on Monday', entity_annotator_DateTime) 
####################### Function 1 : End ######################
###############################################################

###############################################################
## Function 2 : Begin
## Function to get the Part of Speech (POS) tags 
## Input: It expects a string as input.
## Output: Returns a token pair with word/POS combination
###############################################################
tagPOS <- function(x)
{
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  PTA <- Maxent_POS_Tag_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, PTA, a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, '[[', "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  #list(POStagged = POStagged, POStags = POStags)
  return(POStagged)
}
##Usage : 
## tagPos('withdraw money from ATM') 
####################### Function 2 : End ######################
###############################################################

###############################################################
## Function 3 : Begin
## Function to break the POS tags in to 4 major categories 
## namely Noun, Verb, Adverb and Adjective.
## Input: It expects string with POS tags as input.
## Output: Returns a dataframe consisting of 
##         words with only 4 main tags. (ie., Complete tags
##         such 'NOUN' instead of 'NN'
###############################################################
modifyPOS_tags <- function(string)
{ 
  POS_Tags<- tagPOS(string)
  POS_Tags <- strsplit(POS_Tags, ' ')
  POS_Tags <- unlist(POS_Tags)
  POS_Tags <- strsplit(POS_Tags, '/')
  ## creating an empty dataframe
  POS <- data.frame(words=character(0), tags =character(0),stringsAsFactors=FALSE)
  #POS <- rbind(POS_Tags[[1]])
  for(i in 1:length(POS_Tags))
  {
    POS[i,] <- c(POS_Tags[[i]])
    ## NN, NNS, NNP, NNPS <- NOUN
    if(grepl('NN*',POS[i,2], perl = TRUE))
    {
      POS[i,2] <- 'NOUN'
    } else if(grepl('RB*',POS[i,2], perl = TRUE))
    { ## RB, RBR, RBS <- Adverb
      POS[i,2] <- 'ADVERB'
    } else if(grepl('JJ*',POS[i,2], perl = TRUE))
    { #JJ, JJR, JJS <- Adjective
      POS[i,2] <- 'ADJECTIVE'
    } else if(grepl('VB*',POS[i,2], perl = TRUE))
    { ##VB, VBD, VBG, VBN, VBP, VBZ <- Verb
      POS[i,2] <- 'VERB'
    } else
    {
      POS[i,2] <- NA
    }
  }
  POS <- POS[!is.na(POS$tags),]
  return(POS)
}
##Usage : 
## modifyPOS_tags('buy a shoe') 
####################### Function 3 : End ######################
###############################################################

###Need to evaluate
####################### Function 4 : Begin ####################
###############################################################
stemText <- function(string)
{
  ##Storing in a corpus
  value <- Corpus(VectorSource(string))
  #Strip white spaces
  value <- tm_map(value, stripWhitespace)
  #Converting to lower-case
  value <- tm_map(value, tolower)
  #Remove Punctuation
  value <- tm_map(value, removePunctuation)
  #Remove Stopwords
  value <- tm_map(value, removeWords, stopwords("english"))
  #Stemming
  value <- tm_map(value, stemDocument,language = "english")
  #Remove extra spaces
  value <- gsub("\\s+"," ",value[[1]])
  return(value)
}
####################### Function 4 : End #####################
##############################################################