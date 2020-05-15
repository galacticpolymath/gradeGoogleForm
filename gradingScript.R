# Set up grading class structure & define function methods to grade
require(tidyverse);require(stringr)

##
# Delete all spaces in a string to make matching more flexible to mistakes
ds<-function(x){gsub(" ","",x,fixed=T)}


###
# prepKey function processes each question row of Key, creating a list for each entry w/ relevant variables & class according to question type to help gradingAssistant know how to score each question
prepKey<-function(key,mapping){
    #a bit awkward, but ensures 1:1 mapping of questions in key and responses, avoiding order issues in key row entries
    k=key[match(mapping$questionMapping["keyOrig",],key$Field),]
    k$Type<-tolower(k$Type)
   
Qs<-apply(k,1,function(ROW){
    if("Notes"%in%names(k)){notes=as.character(ROW["Notes"])}else{notes=""}
    if("PossiblePoints"%in%names(k)){pts=as.numeric(ROW["PossiblePoints"])}else{pts=1}
    choices<-unlist(strsplit(as.character(ROW["Choices"]),split=";",fixed=T)) %>% trimws()
    if(!is.na(sum(suppressWarnings(as.numeric(choices))))){choices<-as.numeric(choices)}#convert numeric to numeric
    correct<-unlist(strsplit(as.character(ROW["CorrectVal.s."]),";",fixed=T)) %>% trimws()
    if(!is.na(sum(suppressWarnings(as.numeric(correct))))){correct<-as.numeric(correct)}#convert numeric to numeric
    Q=list(Qn=ROW["Q."],Question=paste0("Q",sprintf("%02s" ,ROW["Q."]),") ",as.character(ROW["Field"])),Type=as.character(ROW["Type"]),Choices=if(length(choices)==0){NA}else{choices} ,CorrectVal=if(length(correct)==0){NA}else{correct},CorrectValNoSpc=if(length(correct)==0){NA}else{ds(correct)},PossiblePts=pts,Notes=notes)
    class(Q)<-append(class(Q),as.character(ROW["Type"]))
    Q
    })
names(Qs)<-paste0("q",1:length(Qs))
return(Qs)
}

#attempts to match a shorthand argument or set of answers in a key to actual responses. 
#matches first N characters (default=15) of Key Question Fields to column names in Google Form Responses
#outputs index of match, "duplicated" error, or NA if no match
#gFormEntries can be columns of the google form responses or any other vector for matching to key

alignKey<-function(key,gFormResponses,firstNchar=30){
  gFormEntries<-names(gFormResponses)
  idRows.key<-which(key$Category %>% str_detect("ID"))
  gradedRows.key<-which(key$Category %>% str_detect("Q"))
  qs<-key$Field[gradedRows.key]
  #replace special chars with . to match names of CSV headers
  keystr.ing<-str_replace_all(qs, "[[:punct:]]| ", ".") 
  k.abbrv<-sapply(keystr.ing, function(x) substr(x,1,min(firstNchar,nchar(x))),USE.NAMES = F)
  matchCounts=sapply(k.abbrv,function(x) sum(str_detect(gFormEntries,x)),USE.NAMES = F)
  duplicates<-ifelse(length(which(matchCounts>1))==0,NA,which(matchCounts>1))
  noMatch<-ifelse(length(which(matchCounts==0))==0,NA,which(matchCounts==0))
  matches<-sapply(k.abbrv,function(x) str_which(gFormEntries,x)[1],USE.NAMES = F)
  questionMapping<-rbind(keyQ=k.abbrv,keyOrig=qs,respQ=gFormEntries[matches])
  colnames(questionMapping)=paste0("Q",1:length(k.abbrv))
  out<-list(duplicate.index=duplicates,duplicateKeyEntries=ifelse(is.na(duplicates),NA,k.abbrv[duplicates]),keyMismatchToResponse.index=noMatch,keyMismatchToResponse=ifelse(is.na(noMatch),NA,k.abbrv[noMatch]),mapKeyToResponseIndices=matches,questionMapping=questionMapping,labelCols=idRows.key)
  return(out)
}


#Helper function for grade() that does the actual marking, based on the key (which gets prepped by another helper function), responses, & mapping. Methods vary by question class (i.e. multiple choice, vs. short answer, etc)

# Base function definition (for all methods)
gradeItem<-function(keyListItem,respListItem){
  studentX<-1:length(respListItem)
  studentAns<-as.character(respListItem)
  keycorrect<-tolower(keyListItem$CorrectVal)
  keychoices<-tolower(keyListItem$Choices)
  if(keyListItem$Type=="enternumber"|keyListItem$Type=="manual"){corrected <- keyListItem$CorrectVal}else{
    if(is.numeric(keyListItem$CorrectVal)){corrected<-keyListItem$Choices[keyListItem$CorrectVal]}else{
      if(nchar(keycorrect)<=3){corrected <- keyListItem$Choices[startsWith(keychoices,keycorrect)]}else{corrected <- keyListItem$CorrectVal
      } } }
  UseMethod('gradeItem')
}

gradeItem.default<-function(keyListItem,respListItem){
  availMethods<-paste0(gsub("default","",gsub("gradeItem.","",as.vector(methods(gradeItem)),fixed=T),fixed=T),collapse=", ")
  cat(paste0("*****\nFor Q:",keyListItem$Question,"\n\nI don't know how to grade question type= '"),keyListItem$Type,"'\n",rep("-",20),"\n*Check your key & use one of:\n",availMethods)
  out=tibble(Q=keyListItem$Question,studentX=studentX,studentAns=studentAns, correct="ERROR",score="ERROR",outOf="ERROR",scoreFrac="ERROR")
  return(out)
  }

gradeItem.binary<-function(keyListItem,respListItem){
  
  correct=ifelse(tolower(respListItem)==tolower(corrected),"Yes","No")
  score=ifelse(correct=="Yes",1*keyListItem$PossiblePts,0)
  outOf=keyListItem$PossiblePts
  scoreFrac=paste0(score,"/",outOf)
  out<-tibble(Q=keyListItem$Question,studentX=studentX,studentAns=studentAns,correctAns=as.character(corrected), correct=correct,score=score,outOf=outOf,scoreFrac=scoreFrac)
  return(out)
}


gradeItem.enternumber<-function(keyListItem,respListItem){
  
  correct=ifelse(respListItem==corrected,"Yes","No")
  score=ifelse(correct=="Yes",1*keyListItem$PossiblePts,0)
  outOf=keyListItem$PossiblePts
  scoreFrac=paste0(score,"/",outOf)
  out<-tibble(Q=keyListItem$Question,studentX=studentX,studentAns=studentAns,correctAns=corrected, correct=correct,score=score,outOf=outOf,scoreFrac=scoreFrac)
  return(out)
  
}

gradeItem.chooseone<-function(keyListItem,respListItem){
  
  correct=ifelse(ds(respListItem)==ds(corrected),"Yes","No")
  score=ifelse(correct=="Yes",1*keyListItem$PossiblePts,0)
  outOf=keyListItem$PossiblePts
  scoreFrac=paste0(score,"/",outOf)
  out<-tibble(Q=keyListItem$Question,studentX=studentX,studentAns=studentAns,correctAns=as.character(corrected), correct=correct,score=score,outOf=outOf,scoreFrac=scoreFrac)
  return(out)
  
}

gradeItem.choosemultiple<-function(keyListItem,respListItem){
  #remove spaces from responses & answer to improve matching when indels of spaces happen
  corrected.ds<-ds(corrected) %>% tolower()
  keychoices.ds<-ds(keychoices)%>% tolower()
  studentAns.ds<-ds(studentAns)%>% tolower()
  #annoyingly Google concatenates all answers with , so if answers have commas, can't automatically parse responses; need to string match and reconstruct what answers they put
  studentAnsMat<-sapply(keychoices.ds,function(s) grepl(s,studentAns.ds,fixed=T))
  correctMat<-colnames(studentAnsMat) %in% corrected.ds
  #Here, I do not penalize students for marking an incorrect box. They can only gain fractional points
  accuracyMat<-t(apply(studentAnsMat,1,function(x) ifelse(correctMat==x,1,0)))
  numeratorScores<-rowSums(accuracyMat)
  correct=ifelse(numeratorScores==ncol(accuracyMat),"Yes",ifelse(numeratorScores>0,"Partial","No"))
  score=numeratorScores*keyListItem$PossiblePts/ncol(accuracyMat)
  outOf=keyListItem$PossiblePts
  scoreFrac=paste0(score,"/",outOf)
  out<-tibble(Q=keyListItem$Question,studentX=studentX,studentAns=studentAns,correctAns=paste0(corrected,collapse="; "), correct=correct,score=score,outOf=outOf,scoreFrac=scoreFrac)
  return(out)
  
}

gradeItem.checkpoint<-function(keyListItem,respListItem){
  
  corrected <- keyListItem$CorrectVal
     
  correct="Checkpoint Passed"
  score=keyListItem$PossiblePts
  outOf=keyListItem$PossiblePts
  scoreFrac=paste0(score,"/",outOf)
  out<-tibble(Q=keyListItem$Question,studentX=studentX,studentAns=studentAns,correctAns=as.character(corrected), correct=correct,score=score,outOf=outOf,scoreFrac=scoreFrac)
  return(out)
 }

gradeItem.manual<-function(keyListItem,respListItem){
  
  corrected <- keyListItem$CorrectVal
     
  correct="?"
  score="Needs manual score"
  outOf=keyListItem$PossiblePts
  scoreFrac=paste0("?/",outOf)
  out<-tibble(Q=keyListItem$Question,studentX=studentX,studentAns=studentAns,correctAns=as.character(corrected), correct=correct,score=score,outOf=outOf,scoreFrac=scoreFrac)
  return(out)
 }


###
# The main event: generates the big graded sheet to generate reports
grade<-function(key,gFormResponses){
  #Test if key row # = response column #
  test1<-nrow(key)==ncol(gFormResponses)
  #Map key to questions, identifying errors
  mapping<-alignKey(key,gFormResponses)
  keyList<-prepKey(key,mapping)
  respList<-as.list(gFormResponses[,mapping$mapKeyToResponseIndices])
  names(respList)<-names(keyList)
  allScores<-lapply(1:length(keyList),function(i){gradeItem(keyList[[i]],respList[[i]]) %>% mutate_all(as.character)}) %>% bind_rows()
  #bring id cols back in
  studentInfo<-as.tibble(gFormResponses[,mapping$labelCols]) %>% mutate(studentX=as.character(1:nrow(gFormResponses)))
  out<-full_join(studentInfo,allScores,by="studentX")
  out %>% mutate(score=as.numeric(score),outOf=as.numeric(outOf),scoreFracNum=score/outOf)
  }

