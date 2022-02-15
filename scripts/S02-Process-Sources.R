rm(list = ls())
gc()

# source(here("scripts/S11-Read-obo.R"))

library(XML)
library(parallel)
library(git2r)
library(RJSONIO)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(here)
library(ReDaMoR)
##>
mc.cores <- 55
sdir <- here("sources")
ddir <- here("data")

###############################################################################@
## Data model ----
###############################################################################@
load(here("model", "EFO.rda"))
# dm <- model_relational_data()
# save(dm, file = here("model", "EFO.rda"))

###############################################################################@
## Source information ----
###############################################################################@

desc <- RJSONIO::readJSONStream(here("DESCRIPTION.json"))

sourceFiles <- desc$"source files"
sfi_name <- unlist(lapply(
  sourceFiles,
  function(sf){
    toRet <- sf$"name"
    return(toRet)
  }
))

###############################################################################@
## Data from efo.owl ----
###############################################################################@
## Convert OWL to JSON
Sys.setenv(PATH = paste(Sys.getenv("PATH"),"/home/lfrancois/bin/",sep = ":"))
system(paste("robot convert --input ",file.path(sdir,"efo.owl"),
             " --output ",file.path(sdir,"efo.json"), sep = ""))
readJson <- jsonlite::fromJSON(txt = file.path(sdir,"efo.json"))

# checkJson <- do.call(rbind,
#                  lapply(1:nrow(readJson$graphs$nodes[[1]]),
#                         function(i){
#                           ifelse(is.null(readJson$graphs$nodes[[1]]$meta$basicPropertyValues[[i]]),
#                                  pred <-  NA,
#                                  pred <-  unique(readJson$graphs$nodes[[1]]$meta$basicPropertyValues[[i]]$pred))
#                           data.frame(id = readJson$graphs$nodes[[1]]$id[[i]],
#                                      type = readJson$graphs$nodes[[1]]$type[[i]],
#                                      pred = pred,
#                                      stringsAsFactors = F)
#                         }))
# table(checkJson$type)
# table(unique(checkJson$pred[checkJson$type == "CLASS"]) %in% 
        # unique(checkJson$pred[checkJson$pred == "PROPORTY"]))

###########################################
## nodes (id, def, name, xref, label)
nodesJson <- lapply(1:nrow(readJson$graphs$nodes[[1]]),
                      function(i){
                        ## id
                        id <- gsub("_",":",gsub(".*/","",readJson$graphs$nodes[[1]]$id[[i]]))
                        ## Name and Xref
                        xref <- readJson$graphs$nodes[[1]]$meta$xrefs[[i]]$val
                        name <- readJson$graphs$nodes[[1]]$meta$synonyms[[i]]$val
                        ## Definition
                        def <- readJson$graphs$nodes[[1]]$meta$definition[i,"val"]
                        ## df
                        df1 <- data.frame(
                          id = id,
                          def = def,
                          label = readJson$graphs$nodes[[1]]$lbl[[i]],
                          stringsAsFactors = FALSE)
                        if(length(xref)==0){
                          df2 <- NULL
                        }else{
                          df2 <- data.frame(
                            id = id,
                            Xref = xref,
                            stringsAsFactors = FALSE)
                          }
                        if(length(name)==0){
                          df3 <- NULL
                        }else{
                          df3 <- data.frame(
                            id = id,
                            syn = name,
                            stringsAsFactors = FALSE)
                        }
                        return(list(id = df1,xref = df2,syn = df3))
                      }
               )
id <- do.call(rbind, lapply(nodesJson, function(x) x$id))
xref <- do.call(rbind, lapply(nodesJson, function(x) x$xref))
syn <- do.call(rbind, lapply(nodesJson, function(x) x$syn))

## edges (parents)
edgesJson <- readJson$graphs$edges[[1]]
edgesJson <- edgesJson[which(edgesJson$pred %in% c("is_a")),]
edgesJson$sub <- gsub("_",":",gsub(".*/","",edgesJson$sub))
edgesJson$obj <- gsub("_",":",gsub(".*/","",edgesJson$obj))

## get parents from OBO file
# obo <- getOboInfo("../sources/efo/efo.obo")
# ## Not the same amount of terms in obo and owl
# nf.obo <- id[!(id$id %in% obo$termDef$id),]
# w <- which(!is.na(as.numeric(gsub("^[^[:digit:]]*","",nf.obo$id))))
# table(gsub(":.*","",nf.obo[w,"id"]))
# length(gsub(":.*","",nf.obo[w,"id"]))
# head(nf.obo[w,])
# 
# nf.owl <- obo$termDef[!(obo$termDef$id %in% id$id),]
# ww <- which(!is.na(as.numeric(gsub("^[^[:digit:]]*","",nf.owl$id))))
# table(gsub(":.*","",nf.owl[ww,"id"]))
# head(nf.owl[ww,])

## Get Parents
# edgesJson <- obo$termParents
## Remove NA in parent
# edgesJson <- edgesJson[!is.na(edgesJson$parent),]

getDescendants <- function(id){
  direct <- edgesJson[which(edgesJson$obj == id),"sub"]
  descendants <- direct
  level <- 0
  dLev <- c()
  for(d in direct){
    dDesc <- getDescendants(d)
    dLev <- c(dLev, dDesc$level)
    descendants <- c(descendants, dDesc$descendants)
  }
  if(length(dLev)>0){
    level <- max(dLev)+1
  }
  return(list(descendants=unique(c(descendants, id)), level=level))
}
disease <- getDescendants("EFO:0000408")
length(disease$descendants) ## 9191
"EFO:0000408" %in% disease$descendants
## length(disease$descendants) ## About 200 IDs are missing parent information (are not a child themselves)
## Check missing ids: EFO:0003769 and EFO:0002916
# c("EFO:0003769","EFO:0002916") %in% disease$descendants ## resolved ok

# getDescendants <- function(sp){
#   direct <- edgesJson[which(edgesJson$parent == sp),"id"]
#   descendants <- direct
#   level <- 0
#   dLev <- c()
#   for(d in direct){
#     dDesc <- getDescendants(d)
#     dLev <- c(dLev, dDesc$level)
#     descendants <- c(descendants, dDesc$descendants)
#   }
#   if(length(dLev)>0){
#     level <- max(dLev)+1
#   }
#   return(list(descendants=unique(c(descendants,sp)), level=level))
# }
# diseaseO <- getDescendants("EFO:0000408")
# "EFO:0000408" %in% diseaseO$descendants
# # length(diseaseO$descendants) ## 9209 ## About 200 IDs are missing parent information (are not a child themselves)
# ## Check missing ids: EFO:0003769 and EFO:0002916
# c("EFO:0003769","EFO:0002916") %in% diseaseO$descendants

# getAncestor <- function(id){
#   direct <- edgesJson[which(edgesJson$sub == id),"obj"]
#   ancestors <- direct
#   level <- 0
#   dLev <- c()
#   for(d in direct){
#     dDesc <- getAncestor(d)
#     dLev <- c(dLev, dDesc$level)
#     ancestors <- c(ancestors, dDesc$ancestors)
#   }
#   if(length(dLev)>0){
#     level <- max(dLev)+1
#   }
#   return(list(ancestors=unique(ancestors), level=level))
# }
# test <- getAncestor("EFO:1001901")

######################################
## crossId
crossId <- xref[xref$id %in% disease$descendants,] %>% as_tibble()
dim(crossId)
table(gsub(":.*","",crossId$id))
names(crossId) <- c("dbid1","dbid2")
## Remove spaces
head(grep(": ",crossId$dbid2,value = T))
head(grep(": ",crossId$dbid1,value = T))
crossId$dbid1 <- gsub(" ","", crossId$dbid1)
crossId$dbid2 <- gsub(" ","", crossId$dbid2)
dim(crossId)
##
crossId$DB2 <- gsub(":.*","",crossId$dbid2)
crossId$DB1 <- gsub(":.*","",crossId$dbid1)
crossId$id2 <- gsub(".*:","",crossId$dbid2)
crossId$id1 <- gsub(".*:","",crossId$dbid1)
dim(crossId)
## remove ids with spaces
## keep output copy paste
## Remove crossIds without a colon (e.g. definitions, ...)
head(grep(":",crossId$dbid1,invert = T,value = T))
head(grep(":",crossId$dbid2,invert = T,value = T))
crossId <- crossId[grepl(":",crossId$dbid2) & grepl(":",crossId$dbid1) ,]
dim(crossId)
##
## an integer is a correct disease ID
table(!is.na(as.numeric(crossId$id2)))
table(!is.na(as.numeric(crossId$id1)))
toKeep <- crossId[which(!is.na(as.numeric(crossId$id2)) &
                           !is.na(as.numeric(crossId$id1))),]
dim(toKeep)
toCheck <- crossId[-which(!is.na(as.numeric(crossId$id2)) &
                           !is.na(as.numeric(crossId$id1))),]
dim(toCheck)
## When removing prefix, an integer is a correct disease ID
table(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id2))))
table(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id1))))
toKeep <- rbind(toKeep, 
                toCheck[which(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id2))) &
                                !is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id1)))),])
dim(toKeep)
toCheck <- toCheck[-which(!is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id2))) &
                           !is.na(as.numeric(sub("^[^[:digit:]]*", "", toCheck$id1)))),]
dim(toCheck)

## Remove any DBs that are not disease DBs and DB1 can only be "EFO" or "Orphanet"
## check wrong IDs, remove weird ones still
table(toCheck$DB2)
table(toCheck$DB1)
toCheck[toCheck$DB2 == "ICD9",]
toCheck[toCheck$DB2 == "ICD10",]
toCheck[toCheck$DB2 == "https",]
toCheck[toCheck$DB2 == "ISBN",]
toCheck[toCheck$DB2 == "MSH",]
head(toCheck[toCheck$DB2 == "Wikipedia",])
table(toKeep$DB2)
table(toKeep$DB1)
## Remove xref
toKeep <- toKeep %>% filter(!DB2 %in% c("http", "https", "ISBN-10", "url", "Wikidata", "Wikipedia"))
##
crossId <- setNames(toKeep[,c("dbid1","dbid2")],c("id1","id2"))
dim(crossId)
head(crossId)


## correct ORDO:Orphanet_ wrong encodings
crossId$id2 <- gsub(paste("Orphanet_","Orphanet","ORDO:",sep = "|"),"ORPHA:",crossId$id2)
crossId$id2 <- gsub("MESH","MeSH",crossId$id2)
crossId$id2 <- gsub("MSH","MeSH",crossId$id2)
crossId$id2 <- gsub("MEDGEN","MedGen",crossId$id2)
crossId$id2 <- gsub("MEDDRA","MedDRA",crossId$id2)
crossId$id2 <- gsub("MeDRA","MedDRA",crossId$id2)
crossId$id2 <- gsub("NCI_Thesaurus","NCIt",crossId$id2)
crossId$id2 <- gsub("NCiT","NCIt",crossId$id2)
crossId$id2 <- gsub("NCIT","NCIt",crossId$id2)
crossId$id2 <- gsub("\\bNCI\\b","NCIt",crossId$id2)
crossId$id2 <- gsub("SNOWMEDCT","SNOMEDCT",crossId$id2)
crossId$id2 <- gsub("SNOMEDCT_US","SNOMEDCT",crossId$id2)
crossId$id2 <- gsub("SCTID","SNOMEDCT",crossId$id2)
crossId$id2 <- gsub(paste("UMLSCUI","UMLS_CUI",sep = "|"), "UMLS",crossId$id2)
crossId$id1 <- gsub("Orphanet","ORPHA",crossId$id1)
table(gsub(":.*","",crossId$id1))
table(gsub(":.*","",crossId$id2))
crossId$DB2 <- gsub(":.*","",crossId$id2)
crossId$DB1 <- gsub(":.*","",crossId$id1)

## Remove self references
crossId[which(crossId$id1 == crossId$id2),]
dim(crossId)
crossId <- crossId[-which(crossId$id1 == crossId$id2),]
dim(crossId)

######################################
## entryId
entryId <- id[id$id %in% disease$descendants,] %>% as_tibble()
head(entryId)
dim(entryId)
table(gsub(":.*","",entryId$id))
## Empty definition to NA
nc <- nchar(entryId$def)
head(table(nc), n = 20)
entryId[which(nc < 16),]
entryId[which(nc < 16),"def"] <- entryId[which(nc < 16),"label"]
## Check characters for \t, \n, \r and put to ASCII
entryId$def <- iconv(x = entryId$def,to="ASCII//TRANSLIT")
entryId$def <- gsub(paste("\n","\t","\r", sep = "|")," ",entryId$def)
## Change " to '
entryId$def <- gsub("\"","'",entryId$def)
entryId$def <- gsub("\\\\","",entryId$def)
table(unlist(sapply(entryId$def, strsplit, split = "")))

table(gsub(":.*","",entryId$id))
entryId$id <- gsub("Orphanet","ORPHA",entryId$id)
entryId$DB <- gsub(":.*","",entryId$id)
entryId <- entryId[,c("DB","id","def")]
table(gsub(":.*","",entryId$id))

## Check duplicated records
dim(entryId)
length(unique(entryId[,"id"]))

## all crossId$id1 in entryId
table(crossId$id1 %in% entryId$id)

######################################
## idNames
idNames <- syn[syn$id %in% disease$descendants,]
idNames$canonical <- FALSE
head(idNames)
dim(idNames)
table(gsub(":.*","",idNames$id))

## Labels
lbl <- id[id$id %in% disease$descendants,c("id","label")]
lbl$canonical <- TRUE
table(gsub(":.*","",lbl$id))
unique(grep("#",lbl$id, value =T))
head(lbl)
table(gsub(":.*","",lbl$id))
# lbl <- lbl[grep("#",lbl$id,invert = T, value = F),]

## 
idNames <- idNames %>%
  as_tibble() %>%
  bind_rows(lbl %>% select(id, syn = label, canonical)) %>%
  mutate(id = str_replace(id, "Orphanet", "ORPHA")) %>%
  mutate(DB = gsub(":.*","", id))
## unique
dim(unique(idNames))
idNames <- idNames[order(idNames$canonical,decreasing = T),]
idNames <- unique(idNames)
dim(idNames)

## Check characters for \t, \n, \r and put to ASCII
idNames$syn <- iconv(x = idNames$syn,to="ASCII//TRANSLIT")
idNames$syn <- gsub(paste("\n","\t","\r", sep = "|")," ",idNames$syn)
## Change " to '
idNames$syn <- gsub("\"","'",idNames$syn)
idNames$syn <- gsub("\\\\","",idNames$syn)
table(unlist(sapply(idNames$syn, strsplit, split = "")))
## Remove empty syn
table(is.na(idNames$syn))
## idNames <- idNames[!is.na(idNames$syn),]
dim(idNames)

## all idNames in entryId
table(idNames$id %in% entryId$id)

## Remove empty names, ifany
nc <- nchar(idNames$syn)
table(nc)
head(idNames[which(nc == 6),])
head(idNames[which(nc < 15 & idNames$canonical == FALSE),])
## Remove names of 0 or 1 character long
idNames[which(nc == 0),]
idNames[which(nc == 2),]
# idNames <- idNames[-which(nc == 0),]

## All idnames in entryid
table(idNames$id %in% entryId$id)
table(entryId$id %in% idNames$id)

## Not every ID has a definition available, in this case, the canonical label will be used
tmp <- idNames %>% filter(canonical)
entryId <- entryId %>% 
  mutate(def = case_when(is.na(def) ~ tmp$syn[match(id,tmp$id)],
                         TRUE ~ def))

######################################
## parentId
parentId <- edgesJson[which(edgesJson$obj %in% disease$descendants),c("sub","obj")]
# parentId <- edgesJson[which(edgesJson$parent %in% disease$descendants),]
names(parentId) <- c("id","parent")
table(gsub(":.*","",parentId$id))
table(gsub(":.*","",parentId$parent))
parentId$id <- gsub("Orphanet","ORPHA",parentId$id)
parentId$parent <- gsub("Orphanet","ORPHA",parentId$parent)
parentId$DB <- gsub(":.*","",parentId$id)
parentId$pDB <- gsub(":.*","",parentId$parent)
parentId$origin <- "EFO"

## All idnames in entryid
table(parentId$id %in% entryId$id)
table(parentId$parent %in% entryId$id)
## "Disease" itself is not in entryId --> OK
ep <- parentId[!(parentId$parent %in% entryId$id),]
# ep[ep$parent == "EFO:1001901",]

## Add levels
getAncestors <- function(id){
  direct <- termParents[[id]]
  parents <- direct
  level <- 0
  dLev <- c()
  for(d in direct){
    dPar <- getAncestors(d)
    dLev <- c(dLev, dPar$level)
    parents <- c(parents, dPar$parents)
  }
  if(length(dLev)>0){
    level <- max(dLev)+1
  }
  return(list(parents=unique(parents), level=level))
}

parentList <- unstack(parentId, parent~id)
termParents <- parentList
library(BiocParallel)
bpparam <- MulticoreParam(workers = 30)

termAncestors <- bplapply(
  parentId$id,
  getAncestors,
  BPPARAM = bpparam
)
names(termAncestors) <- parentId$id

entryId <- entryId %>%
  mutate(
    level=unlist(lapply(termAncestors, function(x) x$level))[entryId$id]
  ) %>%
  mutate(level = case_when(is.na(level) ~ 0,
                           TRUE ~ level)) 

#######################################
crossId$id1 <- gsub(".*:","",crossId$id1)
crossId$id2 <- gsub(".*:","",crossId$id2)
entryId$id <- gsub(".*:","",entryId$id)
parentId$id <- gsub(".*:","",parentId$id)
parentId$parent <- gsub(".*:","",parentId$parent)
idNames$id <- gsub(".*:","",idNames$id)

############################
EFO_idNames <- idNames[,c("DB","id","syn","canonical")]
EFO_parentId <- parentId[,c("DB","id","pDB","parent","origin")]
EFO_crossId <- crossId[,c("DB1","id1","DB2","id2")]
EFO_entryId <- entryId[,c("DB","id","def","level")]

############################
toSave <- grep("^EFO[_]", ls(), value=T)
for(f in toSave){
  message(paste("Saving", f))
  print(file.path(ddir, paste(f, ".txt", sep="")))
  ## Ensure unicity
  assign(f, get(f))
  if(length(names(f))==0){
    f <- unique(f)
  }
  ##
  write.table(
    get(f),
    file=file.path(ddir, paste(f, ".txt", sep="")),
    sep="\t",
    row.names=FALSE, col.names=TRUE,
    quote=TRUE,
    qmethod = "double"
  )
}

##############################################################
## Check model
# source("../../00-Utils/autoCheckModel.R")
