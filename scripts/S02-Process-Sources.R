rm(list = ls())
gc()

setwd("~/Shared/Data-Science/Data-Source-Model-Repository/EFO/scripts/")

library(XML)
library(parallel)
library(git2r)
library(RJSONIO)

##
mc.cores <- 55
sdir <- "../sources/efo"
ddir <- "../data"

###############################################################################@
## Source information ----
###############################################################################@

desc <- RJSONIO::readJSONStream("../DESCRIPTION.json")

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
if(!file.exists(file.path(sdir,"efo.json"))){
  Sys.setenv(PATH = paste(Sys.getenv("PATH"),"/home/lfrancois/bin/",sep = ":"))
  system(paste("robot convert --input ",file.path(sdir,"efo.owl"),
               " --output ",file.path(sdir,"efo.json"), sep = ""))
}
readJson <- jsonlite::fromJSON(txt = file.path(sdir,"efo.json"))

###########################################
## nodes (id, def, name, xref, label)
nodesJson <- do.call(rbind,
                     lapply(1:nrow(readJson$graphs$nodes[[1]]),
                            function(i){
                              ## id
                              id <- gsub("_",":",gsub(".*/","",readJson$graphs$nodes[[1]]$id[[i]]))
                              ## Name and Xref
                              descr <- readJson$graphs$nodes[[1]]$meta$basicPropertyValues[[i]]
                              Xref <- paste(descr[grep("_definition_citation",descr$pred),c("val")],collapse = ", ")
                              name <- paste(descr[grep(paste("alternative_term",
                                                             "http://www.ebi.ac.uk/efo/bioportal_provenance",
                                                             sep="|"),
                                                       descr$pred),c("val")],collapse = ", ")
                              ## Definition
                              def <- readJson$graphs$nodes[[1]]$meta$definition[i,"val"]
                              ## df
                              df <- data.frame(
                                id = id,
                                Xref = Xref,
                                def = def,
                                name = name,
                                label = readJson$graphs$nodes[[1]]$lbl[[i]],
                                stringsAsFactors = FALSE)
                              return(df)
                            }
                     )
)
nodesJson$id <- gsub("Orphanet","ORPHA",nodesJson$id)
nodesJson$id <- gsub("MSH","MeSH",nodesJson$id)
nodesJson$id <- gsub("NCI","NCIt",nodesJson$id)
nodesJson$id <- gsub("ORDO","ORPHA",nodesJson$id)

## edges (parents)
edgesJson <- readJson$graphs$edges[[1]]
edgesJson <- edgesJson[which(edgesJson$pred %in% c("is_a")),]
edgesJson$sub <- gsub("_",":",gsub(".*/","",edgesJson$sub))
edgesJson$obj <- gsub("_",":",gsub(".*/","",edgesJson$obj))

######################################
## crossId
crossId <- unique(nodesJson[,c("id","Xref")])
crossIdList <- strsplit(crossId$Xref, split = ",")
names(crossIdList) <- crossId$id
crossId <- stack(crossIdList)
names(crossId) <- c("id2","id1")
crossId <- crossId[!grepl("#",crossId$id1),]
crossId$id2 <- gsub("ICD-10","ICD10",crossId$id2)
crossId$id2 <- gsub("MESH","MeSH",crossId$id2)
crossId$id2 <- gsub("MSH","MeSH",crossId$id2)
crossId$id2 <- gsub("NCI_Thesaurus","NCIt",crossId$id2)
crossId$id2 <- gsub("NCiT","NCIt",crossId$id2)
crossId$id2 <- gsub("NCIT","NCIt",crossId$id2)
crossId$id2 <- gsub("ORDO","ORPHA",crossId$id2)
crossId$id2 <- gsub("SNOWMEDCT","SNOMEDCT",crossId$id2)
crossId$id2 <- gsub("UMLS","MedGen",crossId$id2)
crossId$DB2 <- gsub(":.*","",crossId$id2)
crossId$DB1 <- gsub(":.*","",crossId$id1)

######################################
## entryId
entryId <- nodesJson["id"]
entryId <- entryId[grep("#",entryId$id, invert = T, value = F),,drop = FALSE]
entryId$DB <- gsub(":.*","",entryId$id)
entryId <- entryId[,c("DB","id")]
entryId$definition <- nodesJson$def[match(entryId$id,nodesJson$id)]
entryId$definition <- ifelse(entryId$definition == "NA",NA,entryId$definition)
entryId$definition <- tolower(entryId$definition)
entryId$definition <- gsub("[[:punct:]]"," ",entryId$definition)
entryId$definition <- iconv(x = entryId$definition,to="ASCII//TRANSLIT")
entryId$definition <- gsub("\n"," ",entryId$definition)

######################################
## idNames
idNames <- unique(nodesJson[,c("id","name")])
idNamesList <- strsplit(idNames$name, split = ",")
names(idNamesList) <- idNames$id
idNames <- stack(idNamesList)
names(idNames) <- c("name","id")
idNames <- idNames[grep("#",idNames$id,invert = T, value = F),,drop = FALSE]
## Labels
lbl <- unique(nodesJson[,c("label","id")])
lbl <- lbl[grep("#",lbl$id,invert = T, value = F),]
## 
idNames <- rbind(idNames,setNames(lbl, nm = names(idNames)))
idNames$DB <- gsub(":.*","",idNames$id)
idNames$canonical <- ifelse(idNames$name %in% lbl$label, TRUE, FALSE)
idNames$name <- tolower(idNames$name)
idNames$name <- gsub("[[:punct:]]"," ",idNames$name)
idNames$name <- iconv(x = idNames$name,to="ASCII//TRANSLIT")
idNames$name <- gsub("\n"," ",idNames$name)
idNames <- unique(idNames)

######################################
## parentId
parentId <- edgesJson[grep("oboInOwl#ObsoleteClass",edgesJson$obj, invert = T),c("sub","obj")]
names(parentId) <- c("id","parent")
parentId$id <- gsub("Orphanet","ORPHA",parentId$id)
parentId$parent <- gsub("Orphanet","ORPHA",parentId$parent)
parentId$DB <- gsub(":.*","",parentId$id)
parentId$pDB <- gsub(":.*","",parentId$parent)
parentId <- parentId[parentId$id %in% nodesJson$id,]
parentId <- parentId[parentId$parent %in% nodesJson$id,]

#######################################
crossId$id1 <- gsub(".*:","",crossId$id1)
crossId$id2 <- gsub(".*:","",crossId$id2)
entryId$id <- gsub(".*:","",entryId$id)
parentId$id <- gsub(".*:","",parentId$id)
parentId$parent <- gsub(".*:","",parentId$parent)
idNames$id <- gsub(".*:","",idNames$id)

############################
EFO_idNames <- idNames[,c("DB","id","name","canonical")]
EFO_parentId <- parentId[,c("DB","id","pDB","parent")]
EFO_crossId <- crossId[,c("DB1","id1","DB2","id2")]
EFO_entryId <- entryId[,c("DB","id","definition")]

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
    sep="|",
    row.names=FALSE, col.names=TRUE,
    quote=FALSE
  )
}
