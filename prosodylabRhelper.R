


whichColumnsVary <- function(df,
                             idVariables = c("experiment","participant","item","condition"),
                             timeVariable = "ioiLabel"
                             ){
  #
  require(dplyr)
  
  # Check whether union(idVariables,timeVariable) uniquely identify rows
  #
  doublets = checkUniqueness(df,union(idVariables,timeVariable))
  if (nrow(doublets)!=0){
    warning(
      cat('idVariables + timeVariable do not uniquely identify rows.\n', 
          'Check with: checkUniqueness(df, ', deparse(union(idVariables,timeVariable)),'))')
      )
  }
  
  #
  # Check which columns vary and return those column names
  otherColumns=setdiff(names(df),union(idVariables,timeVariable))
  varyingColumns = df %>% group_by_at(idVariables) %>%
    dplyr::mutate_at(otherColumns,n_distinct) %>% 
    as.data.frame %>%
    dplyr::select(-one_of(union(idVariables,timeVariable))) %>%
    dplyr::select(which(colMeans(.) > 1)) %>%
    names()

  return(varyingColumns)
  
  # example
  # for reshape, cherck output columns vary by id variables
  # example:
  # idVariables=c("recordedFile")
  # timeVariable=c("Syllable")
  # both.wide=stats::reshape(both,
  #                          idvar=idVariable,
  #                          timevar=timeVariable,
  #                          v.names=whichColumnsVary(both,idVariables,timeVariable),
  #                          direction="wide")
  #
}   

# check uniqueness of id columns
checkUniqueness <- function(df,idVariables=c('experiment','item','condition','participant','ioiLabel')){
  
  # add filename to the output if it's a column
  if ("recordedFile" %in% colnames(df)) {
    idVariables = union(idVariables,"recordedFile")
  }
  if ("fileName" %in% colnames(df)) {
    idVariables = union(idVariables,"fileName")
  }
  
  nonUniqueRows = df %>%
    group_by_at(idVariables) %>%
    summarise(count = n()) %>%
    filter(count>1) %>%
    as.data.frame()
  
  return(nonUniqueRows)
  
}


convertVariables <- function(df) {
  # columns that are usually read as factors but should be numeric:
  numericColMatlab = c("trialDuration")
  
  numericColPraatscript = c("rIntensity","rPitch","rDuration","duration", "silence", "duraSil", "phoneLength", "meanPitch", "maxPitch", "maxPitTime", "minPitch", "minPitTime", "pitch1", "pitch1_time", "pitch2", "pitch2_time", "pitch3", "pitch3_time", "pitch4", "pitch4_time", "pitch5", "pitch5_time", "pitch6", "pitch6_time", "pitch7", "pitch7_time", "pitch8", "pitch8_time", "pitch9", "pitch9_time", "pitch10", "pitch10_time", "meanIntensity", "maxIntensity", "maxIntTime", "intensity1", "intensity1_time", "intensity2", "intensity2_time", "intensity3", "intensity3_time", "intensity4", "intensity4_time", "intensity5", "intensity5_time", "intensity6", "intensity6_time", "intensity7", "intensity7_time", "intensity8", "intensity8_time", "intensity9", "intensity9_time", "intensity10", "intensity10_time", "zstart", "zend", "zDuration", "zPhonelength", "zmeanPitch", "zmaxPitch", "zmaxPitTime", "zminPitch", "zminPitTime", "zmeanIntensity", "zmaxIntensity", "zmaxIntTime", "response", "duration", "silence", "durasil", "meanpitch", "maxpitch", "maxPitTime", "minPitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity", "zduration", "zbeginzone", "zendzone", "zphonelength", "zmeanpitch", "zmaxpitch", "zmaxPitTime", "zminPitch", "zminPitTime", "zfirstpitch", "zsecondpitch", "zthirdpitch", "zfourthpitch", "zmeanIntensity", "zmaxIntensity", "durasil", "meanpit", "maxpitch", "maxPitTime", "minpitch", "minPitTime", "firstpitch", "secondpitch", "thirdpitch", "fourthpitch", "meanIntensity", "maxIntensity", "firstF1", "firstF2", "firstdif", "secondF1", "secondF2", "seconddif", "thirdF1", "thirdF2", "thirddif", "fourthF1", "fourthF2", "fourthdif", "fifthF1", "fifthF2", "fifthdif")
  
  numericColJspsychExperimenter = c("trial_index","time_elapsed","rt","correct","headPhoneScreenerScore") 
  
  numeriColOther = c("F1","F2")
  
  numericCols = c(numericColMatlab,numericColPraatscript, numericColJspsychExperimenter)
  
  nColumns = ncol(df)
  # convert to numeric column, otherwise treat as factor:
  for (i in 1:nColumns) {
    if (colnames(df)[i] %in% numericCols) {
      df[, i] <- as.numeric(as.character(df[, i]))
    } else {
      df[, i] <- as.factor(as.character(df[, i]))
    }
  }
  return(df)
}


importData <- function(pathData,pathStudyFile) {
  
  require(jsonlite)
  require(tidyverse)
  
  studyFile = read.csv(pathStudyFile,
                       sep="\t", header=TRUE) %>% convertVariables()
  
  # create a list of the "data*"  files from your target directory
  fileList <- list.files(path=pathData,pattern="data*")
  # keep only .json files
  fileList  = Filter(function(x) grepl(".json", x), fileList)
  
  d <- data.frame()
  
  # load in  data files  from all participants
  for (i in 1:length(fileList)){
    #print(fileList[i])
    #paste0('data/',fileList[i])
    tempData = fromJSON(paste0(pathData,'/',fileList[i]), flatten=TRUE)
    
    # this line replaces NA for experiments where due to a bug components other than the test trials didn't have participant number added ot them
    tempData$participant = unique(tempData$participant[!is.na(tempData$participant)])
    tempData$pList = as.character(tempData$pList)
    d <- bind_rows(d,tempData)
  }
  
  # tempData = fromJSON(paste0(pathData,'/','data_5f87791cf3c64e1a1f313d66.json'), flatten=TRUE)
  
  # initiate data frame  with participant information
  participants <- data.frame(participant = unique(d$participant))
  
  # questionnaire data:
  # how to convert json cell into  columns (there might be an easier way using  jsonlite more directly?): https://stackoverflow.com/questions/41988928/how-to-parse-json-in-a-dataframe-column-using-r
  
  #  debriefing questionnaire data:
  if (nrow(filter(d,component=='Post-experiment Questionnaire'))!=0){
    participants <- d %>% 
      filter(component=='Post-experiment Questionnaire') %>% 
      dplyr::select(c(participant,responses)) %>%
      mutate(responses = map(responses, ~ fromJSON(.) %>% 
                               as.data.frame())) %>% 
      unnest(responses) %>% 
      right_join(participants, by = c("participant"))
  }
  
  #  music  questionnaire data:
  if (nrow(filter(d,component=='Music Questionnaire'))!=0){
    participants <- d %>% 
      filter(component=='Music Questionnaire') %>% 
      dplyr::select(c(participant,responses)) %>%
      mutate(responses = map(responses, ~ fromJSON(.) %>% 
                               as.data.frame())) %>% 
      unnest(responses) %>% 
      right_join(participants, by = c("participant"))
  }
  
  if (nrow(filter(d,component=='Language Questionnaire'))!=0){
    participants <- d %>% 
      filter(component=='Language Questionnaire') %>% 
      dplyr::select(c(participant,responses)) %>%
      mutate(responses = map(responses, ~ fromJSON(.) %>% 
                               as.data.frame())) %>% 
      unnest(responses) %>% 
      right_join(participants, by = c("participant"))
  }
  
  if (nrow(filter(d,component=='Headphone screener'))!=0){
    participants = d %>% 
      filter(component=='Headphone screener'&grepl("Headphone screener question",trialPart)) %>%
      mutate(correct = as.numeric(as.character(correct))) %>%
      group_by(participant) %>%
      summarise(headPhoneScreenerScore=mean(correct)) %>%
      as.data.frame %>%
      right_join(participants, by = c("participant"))
  }
  
  d <- d %>% filter(component=='Experiment') %>%
    # combine  with participant data
    left_join(participants,by = c("participant")) %>%
    # turn empty strings (e.g., "",  '',  "  ") into NA
    apply(2, function(x) gsub("^$|^ $", NA, x))  %>%
    as.data.frame %>% convertVariables()
  
  d = left_join(d,studyFile,by=c("experiment","item","condition")) ## %>%
    #filter(!is.na(chosenOption))
  
  return(d)
  
}


reportComparison = function(model,factorName) {
  # assumes  that last  coefficiient is p-value
  # e.g. from  lmertest for lmer
  nCoefficients = length(colnames(summary(model)$coefficients))
  pValName = colnames(summary(model)$coefficients)[nCoefficients]
  
  output = paste0(
    "$\\beta$ = ", round(coef(summary(model))[factorName,'Estimate'], 2), # β
    "; s.e. = ", round(coef(summary(model))[factorName,'Std. Error'], 2),
    "; p $<$ ", max(round(coef(summary(model))[factorName,pValName], 2),0.001)
  )
  
  return(enc2utf8(enc2native(output)))
}


getParticipantInformation <- function(participantNumbers){
  
  pathLQ = '/Users/chael/Dropbox/Lab/participants/processedData/lq_shortform_2019.txt'
  lq = read.csv(pathLQ, sep='\t')  %>% 
    filter(participant %in% participantNumbers) %>%
    mutate(participant = factor(participant)) %>%
    dplyr::select("participant", "Timestamp", "Participant", "BirthYear", "Gender", 
                  "Country", "Region", "StateProvince", "City", "Second", "Third", 
                  "Fourth", "French", "French.Which", "French.Fluency", "French.Understanding", 
                  "English", "English.Which", "English.Fluency", "English.Understanding", "NewCorpusConsent", 
                  "FrenchType", "EnglishType")
  
  return(lq)
  
}


addLanguageQuestionnaire = function(dataSet){
  dataset = dataSet %>%
    left_join(getParticipantInformation(dataSet$participant), by = c("participant")) %>%
    convertVariables()
}

getMusicQuestionnaire = function(participantNumbers){
  
  mq = read.csv("/Users/chael/Dropbox/lab/participants/lqArchived/mq_saved_nov_2020.tsv",sep='\t') %>%
    convertVariables()
  
  names(mq)[names(mq) == 'Participant..'] <- 'participant'
  
  mq$How.much.do.you.know.about.music.structure.and.theory. = 
    factor(mq$How.much.do.you.know.about.music.structure.and.theory. ,
           levels = c("Nothing", "A little", "A moderate amouunt", "A fair amount", "A great deal")
    )
  
  mq$How.many.years.of.formal.music.training..practice..have.you.had. = 
    factor(mq$How.many.years.of.formal.music.training..practice..have.you.had.,
           levels = c("None", "1 year",  
                      "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", 
                      "8 years", "9 years", "10+ years")
    )
  
  mq$YearsTrainingNumeric = dplyr::recode(mq$How.many.years.of.formal.music.training..practice..have.you.had.,
                                          "None" = 0, 
                                          "1 year" = 1,
                                          "2 years" = 2, 
                                          "3 years" = 3, 
                                          "4 years" = 4, 
                                          "5 years" = 5, 
                                          "6 years" = 6, 
                                          "7 years" = 7, 
                                          "8 years" = 8, 
                                          "9 years" = 9, 
                                          "10+ years" = 10
  )
  
  # zscore of the years someone had music lessons based on 
  mq$YearsTrainingScaled = arm::rescale(as.numeric(mq$How.many.years.of.formal.music.training..practice..have.you.had.))
  
  mq$How.often.do.you.engage.in.professional.music.making..e.g..singing..playing.an.instrument..composing.. = 
    factor(mq$How.often.do.you.engage.in.professional.music.making..e.g..singing..playing.an.instrument..composing..,
           levels = c("Never", "Rarely", "Sometimes","Often", "All the time")
    )
  
  mq$How.often.did.you.or.do.you.practice.or.rehearse.with.an.instrument.or.singing. = 
    factor(mq$How.often.did.you.or.do.you.practice.or.rehearse.with.an.instrument.or.singing.,
           levels = c("Never", "Rarely", "Sometimes","Often", "All the time")
    )
  
  mq$How.often.do.you.engage.in.music.making.as.a.hobby.or.as.an.amateur. = 
    factor(mq$How.often.do.you.engage.in.music.making.as.a.hobby.or.as.an.amateur.,
           levels = c("Never", "Rarely", "Sometimes","Often", "All the time")
    ) 
  
  
  return(mq %>% filter(participant %in% participantNumbers))
  
}

addMusicQuestionnaire = function(dataSet){
  dataset =   dataSet %>%
    left_join(getMusicQuestionnaire(dataSet$participant), by = c("participant")) %>%
    convertVariables()
  
}


addAnnotation = function(df,fileName,identVariables){
  if (missing(identVariables)){
    identVariables = c("recordedFile")
  } 
  annotationDF = read.csv(fileName,sep='\t')
  df = df %>% 
    left_join(annotationDF, by = identVariables) %>%
    convertVariables()
  
  return(df)
}
# example: d=addAnnotation(d,'dataAcoustics/homphInitialSabrina.txt','recordedFile')


  
addAcoustics = function(df,acousticsFilename,idvariable=c('experiment','item','condition','participant'),timevariable='ioiLabel'){
  
  require("tidyverse")
  options(dplyr.summarise.inform = FALSE)
  
  acoustics = read.csv(acousticsFilename,row.names=NULL,sep='\t') %>% convertVariables()
  
  # check if old extraAcoustics script was used, if so change default timevariable to "woiLabel"  
  if (timevariable == 'ioiLabel'){
    if ("woiLabel" %in% colnames(acoustics)) {
      timevariable = 'woiLabel'
    }
  }

  # correct column name for soundfilename in acoustics file if necessary
  if ('recordedFile' %in% idvariable){
    names(acoustics)[names(acoustics) == 'fileName'] <- 'recordedFile'
  }
  
  # create "recordedFile" column in experiment spreadsheet if necessary
  if (!("recordedFile" %in% colnames(df))) {
    df$recordedFile = paste0(df$experiment,"_",df$participant,"_",df$item,"_",df$condition,".wav")
  }
  
  acoustics = acoustics  %>% 
    convertVariables() %>%
    # filter out lines without ioiLabel
    filter(!is.na({{timevariable}})&!({{timevariable}}=="")) %>% 
    stats::reshape(idvar=idvariable,
                   timevar=timevariable,
                   v.names=whichColumnsVary(acoustics,idvariable,timevariable),
                   direction="wide")
  
  df = df %>%
    convertVariables() %>%
    left_join(acoustics, by = idvariable)
  
  return(df)
}


relativeMeasures = function(df,woi1, woi2,label="") {
 
  labelPitch = paste0("rPitch",label)
  labelDuration = paste0("rDuration",label)
  labelIntensity = paste0("rIntensity",label)
  
  # Relative rations
  # semitones:
  df[labelPitch] = 12*log2(df[,paste0('maxPitch.',woi1)]/ df[,paste0('maxPitch.',woi2)])
  # ratio of durations (difference in log duration):
  df[labelDuration] = log(df[,paste0('duration.',woi1)]) - log(df[,paste0('duration.',woi2)])
  # ratio of loudness (difference of dB):
  df[labelIntensity] = log(df[,paste0('maxIntensity.',woi1)]) - log(df[,paste0('maxIntensity.',woi2)])
  
  # Relative measures
  #df$rPitch=12*log2(df$maxPitch.1/df$maxPitch.2)
  # Relative duration (difference in log duration)
  #df$rDuration=log(df$duration.1)-log(df$duration.2)
  # Relative intensity (difference)
  #df$rIntensity=df$maxIntensity.1-df$maxIntensity.2
  
  return(df)
}
