#devtools::install_github("duckmayr/gpmlr")
library(gpmlr)
library(sentimentIt)
library(readr)
library(httr)
library(dplyr)

setwd('~/Dropbox/AdaptiveText')
source('HelperFunctions/SentimentItCredentials.R')
source('HelperFunctions/DropboxLinks.R')
source('HelperFunctions/SelectComps.R')

load('~/Dropbox/AdaptiveText/Experiment/ActiveLearning_InformativePrior/ActiveInformativeResults.RData')

Prompt <- gsub('\n', '', read_file('TaskFormatting/QuestionPrompt.txt'))

docInfo <- readText(email = username,
                    password = password,
                    read_documents_from = LinksToTurk,
                    index = 'Link')

###
# Active learning: 476 * 20 / 2 total HITs worth of active learning comparisons
#####

# set number of comparisons per batch we want to upload to MTurk
nCompsPerBatch <- 100
# generate all possible combinations of comparisons
CompCombos <- t(combn(docInfo$ids, 2))

# create batch ids for all subsequent batches
# includes initial batch + n HITs in vanilla SentimentIt / n comparisons per batch
batch_ids <- createBatches(email = username,
                           password = password,
                           task_setting_id = 41,
                           num_batches = 45)
#batch ids:

# first generate some score for each doc w/ vanilla SentimentIt
# this was done in a previous attempt, therefore we can load in the old data
batchStatus(username, password, 3067)

# get results from MTurk
Results <- readInData(email = username,
                      password = password,
                      batch_id = 3067)

CurrentComps <- cbind(Results$document_id[seq(1, nrow(Results), 2)],
                      Results$document_id[seq(2, nrow(Results), 2)])

# SelectComps takes y in (-1, 1), where 1 indicates selection of left doc. in pair,
#     -1 indicates right doc. in pair
# ScoredComps reformats the comparisons so selection of top doc == 1 and bottom doc == -1
ScoredComps <- Results$result[seq(1, nrow(Results), 2)] * 2 - 1

# modify this if re-starting at a later iteration
Start <- 1
# do active learning
for(i in Start:length(batch_ids)){
  print(paste0('Running batch ', i))
  
  CurrentComps <- SelectComps(doc_ids = docInfo$ids, 
                              pairs = CompCombos[sort(sample(1:nrow(CompCombos), nrow(CompCombos)/10, replace = F)), ],
                              n_select = nCompsPerBatch, 
                              x = CurrentComps, 
                              y = ScoredComps)
  
  print('Posting to SentimentIt')
  auth_token <- sentimentIt::authenticate(username, password)
  POST("https://www.sentimentit.com/api/comparisons/create.json",
       body = list(
         ids = CurrentComps[(nrow(CurrentComps) - (nCompsPerBatch - 1)):nrow(CurrentComps), ],
         batch_id = batch_ids[i],
         question = Prompt,
         template_id = 6,
         email = username,
         auth_token = auth_token),
       content_type_json(),
       encode = 'json')
  
  Sys.sleep(5)
  Status <- batchStatus(username, password, batch_ids[i])
  print(Status)
  
  while(Status[2] != nCompsPerBatch){
    Sys.sleep(5)
    Status <- batchStatus(username, password, batch_ids[i])
    print(Status)
  }
  
  print('Sending task to MTurk')
  createTasks(email = username,
              password = password,
              batch_id = batch_ids[i])
  
  Sys.sleep(300)
  Status <- batchStatus(username, password, batch_ids[i])
  print(Status)
  
  while(Status[4] != nCompsPerBatch){
    Sys.sleep(60)
    Status <- batchStatus(username, password, batch_ids[i])
    print(Status)
  }
  
  Results <- readInData(email = username,
                        password = password,
                        batch_id = batch_ids[i])
  # multiply new scores by -1 b/c doc order gets switched on MTurk
  ScoredComps <- c(ScoredComps, 
                   (-1 * (Results$result[seq(1, nrow(Results), 2)] * 2 - 1)))
  save.image('~/Dropbox/AdaptiveText/Experiment/ActiveLearning_NaivePrior/ActiveNaiveResults.RData')
  
  if(i %% 25 == 0){
    Continue <- readline(prompt = paste0('Iteration ', i, ' complete. Continue? y/n'))
    if(Continue == 'n'){
      break
    }
  }
}

batchStatus(username, password, batch_ids[41])

AllResults <- readInData(email = username,
                         password = password,
                         batch_id = batch_ids[1:37])
ResultComps <- cbind(AllResults$document_id[seq(2, nrow(AllResults), 2)],
                     AllResults$document_id[seq(1, nrow(AllResults), 2)])
ResultScores <- -1 * (AllResults$result[seq(1, nrow(AllResults), 2)] * 2 - 1)

STAN <- fitStan(data = AllResults)
Workers <- checkWorkers(stan_fit = STAN$fit, data = AllResults)
table(AllResults$worker_id)
Workers