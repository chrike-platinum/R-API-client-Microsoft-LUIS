install.packages('jsonlite')
install.packages("xml2")
install.packages("rjson")
library(jsonlite)
library(httr)
library(stringr)
library(glue)
library(rjson)


"---------------------------------LUIS PARAMETERS---------------------------------"

app_ID<-"ID"
KEY<-"KEY"
HOST<-"https://westus.api.cognitive.microsoft.com"
app_VERSION <- "0.1"
PATH<-"/luis/api/v2.0/apps/{app_id}/versions/{app_version}/"
PUBLISHPATH= "/luis/api/v2.0/apps/{app_id}/"
QUESTIONPATH ="/luis/v2.0/apps/{app_id}/"


"---------------------------------JSON CREATION METHODS---------------------------------"

getSingleJSONElement <- function(sentence,label,order="mid"){
  label <- trimws(label)
  if(order=="first"){
    elements_list <- sprintf('[{"text":"%s", 
                             "intentName": "%s",
                             "entityLabels": "[]"
  },', sentence, label)
  }
  
  if (order=="mid"){
    elements_list <- sprintf('{"text":"%s", 
                             "intentName": "%s",
                             "entityLabels": "[]"
  },', sentence, label)
  }
  if (order=="last"){
    elements_list <- sprintf('{"text":"%s", 
                             "intentName": "%s",
                             "entityLabels": "[]"
  }]', sentence, label)
    
}
  return(elements_list)
}

getSingleJSONIntentElement <- function(label,order="mid"){
  label <- trimws(label)
  if(order=="first"){
    elements_list <- sprintf('[{"name":"%s" 
  },', label)
  }
  
  if (order=="mid"){
    elements_list <- sprintf('{"name":"%s"
  },', label)
  }
  if (order=="last"){
    elements_list <- sprintf('{"name":"%s"
  }]', label)
    
}
  return(elements_list)
}

getSingleJSONPublishElement<-function(AppVersion,isStaging,region){
  return(sprintf('{"versionId":"%s", 
            "isStaging": "%s",
          "region": "%s"}',AppVersion,isStaging,region))
  
}


makeUtteranceFile<-function(fileName,outputFileName,verbose=TRUE){
  MyData <- read.csv(file=fileName, header=FALSE, sep="|")
  JSON<-c()
  JSON<-c(JSON,getSingleJSONElement(MyData[1,'V1'],MyData[1,'V2'],order="first"))
  for (i in 2:NROW(MyData)-1){
    JSON<-c(JSON,getSingleJSONElement(MyData[i,'V1'],MyData[i,'V2']))
  }
  JSON<-c(JSON,getSingleJSONElement(MyData[NROW(MyData),'V1'],MyData[NROW(MyData),'V2'],order="last"))
  if (verbose==TRUE){
    write(JSON,outputFileName)
  }
  
  return(JSON)
}

makeIntentFile<-function(fileName,outputFileName,verbose=TRUE){
  MyData <- read.csv(file=fileName, header=FALSE, sep="|")
  JSON<-c()
  #JSON<-c(JSON,getSingleJSONIntentElement(MyData[1,'V2'],order="first"))
  for (i in 1:NROW(MyData)){
    JSON<-c(JSON,getSingleJSONIntentElement(MyData[i,'V2']))
  }
  #JSON<-c(JSON,getSingleJSONIntentElement(MyData[NROW(MyData),'V2'],order="last"))
  if (verbose==TRUE){
    write(JSON,outputFileName)
  }
  return(JSON)
}

makePublishInfoFile<-function(outputFileName,AppVersion="0.1",isStaging=TRUE,region="westus",verbose=FALSE){
  JSON<-getSingleJSONPublishElement(AppVersion,isStaging,region)
  if (verbose==TRUE){
    write(JSON,outputFileName)
  }
  return(JSON)
}

"---------------------------------LUIS (AUXILARY) COMMUNICATION METHODS---------------------------------"

addIntents<-function(inputFile){
  INTENTFILENAME = "intent.json"
  JSON <- makeIntentFile(inputFile,INTENTFILENAME)
  Path    = PATH
  lUISEndPointIntent = "intents"
  path = paste(Path,lUISEndPointIntent,sep="")
  app_id=app_ID
  app_version=app_VERSION
  path2<-glue(path)
  key = KEY
  host = HOST
  for (intent in JSON){
    r <- POST(paste(host,path2,sep=""),add_headers("Ocp-Apim-Subscription-Key" = key,"content"="json"),body=intent,encode = "json")
    print(content(r))
  }
  
}  

  
addUtterances<-function(inputFile){
UTTERANCEFILENAME = "utterances.json"
INTENTFILENAME = "intent.json"
JSON <- makeUtteranceFile(inputFile,UTTERANCEFILENAME)
Path=PATH
lUISEndPoint = "examples"
path = paste(Path,lUISEndPoint,sep="")
app_id=app_ID
app_version=app_VERSION
path2<-glue(path)
key = KEY
host = HOST
r <- POST(paste(host,path2,sep=""),add_headers("Ocp-Apim-Subscription-Key" = key),body=upload_file(UTTERANCEFILENAME),encode = "json")
print(content(r))
}

trainModel<-function(){
  Path  = PATH
  lUISEndPoint = "train"
  path = paste(Path,lUISEndPoint,sep="")
  app_id=app_ID
  app_version=app_VERSION
  path2<-glue(path)
  key = KEY
  host = HOST
  r <- POST(paste(host,path2,sep=""),add_headers("Ocp-Apim-Subscription-Key" = key))
  print(content(r))
}

getTrainingStatus<-function(){
  Path = PATH
  lUISEndPoint = "train"
  path = paste(Path,lUISEndPoint,sep="")
  app_id=app_ID
  app_version=app_VERSION
  path2<-glue(path)
  key = KEY
  host = HOST
  r <- GET(paste(host,path2,sep=""),add_headers("Ocp-Apim-Subscription-Key" = key))
  print(content(r))
}

publishApplication<-function(){
  publishInfo <- makePublishInfoFile()
  Path = PUBLISHPATH
  lUISEndPoint = "publish"
  path = paste(Path,lUISEndPoint,sep="")
  app_id=app_ID
  app_version=app_VERSION
  path2<-glue(path)
  key = KEY
  host = HOST
  r <- POST(paste(host,path2,sep=""),add_headers("Ocp-Apim-Subscription-Key" = key),body=publishInfo)
  print(content(r))
}
publishApplication()


"---------------------------------PUBLIC METHODS---------------------------------"

addTrainingExamplesToLUIS<-function(fileName){
print('------ADDING INTENTS------')
addIntents(fileName)
print('------ADDING UTTERANCES------')
addUtterances(fileName)
print('------TRAINING MODEL------')
trainModel()
Sys.sleep(10)
print('------CHECKING TRAINING STATUS------')
getTrainingStatus()
print('------PUBLISH APPLICATION FOR USAGE------')
publishApplication()
}

analyseQuestion <- function(question,stagingPar,verbosePar,timezonePar){
  
  Path = QUESTIONPATH
  lUISEndPoint = ""
  path = paste(Path,lUISEndPoint,sep="")
  app_id=app_ID
  app_version=app_VERSION
  path2<-glue(path)
  subscriptionKey= KEY
  requestedSentence=gsub(" ","+",question)
  extra='?subscription-key={subscriptionKey}&staging={stagingPar}&verbose={verbosePar}&timezoneOffset={timezonePar}&q={requestedSentence}' # &staging=true&verbose=true&timezoneOffset=0'
  extra<-glue(extra)
  path2 = paste(path2,extra,sep="")
  key = KEY
  host = HOST
  r<-GET(paste(host,path2,sep=""))#,add_headers('Ocp-Apim-Subscription-Key' = key),params=list('staging'='true','verbose'='true',timezoneOffset=0),query=c(q="test"))
  print((content(r)$topScoringIntent))
  print((content(r)$entities))


}




"---------------------------------EXAMPLE USAGE---------------------------------"
addTrainingExamplesToLUIS("LOCATION TO TEXT FILE WITH SENTENCES AND LABELS lines (SEP=|)")
analyseQuestion("INSERTQUESTION HERE?",TRUE,TRUE,0)






