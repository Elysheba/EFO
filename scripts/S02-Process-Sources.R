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
entryId$DB <- gsub(":.*","",entryId$id)
entryId <- entryId[,c("DB","id")]
entryId$definition <- nodesJson$def[match(entryId$id,nodesJson$id)]

######################################
## idNames
idNames <- unique(nodesJson[,c("id","name")])
idNamesList <- strsplit(idNames$name, split = ",")
names(idNamesList) <- idNames$id
idNames <- stack(idNamesList)
names(idNames) <- c("name","id")
## Labels
lbl <- unique(nodesJson[,c("label","id")])
## 
idNames <- rbind(idNames,setNames(lbl, nm = names(idNames)))
idNames$DB <- gsub(":.*","",idNames$id)
idNames$canonical <- ifelse(idNames$name %in% lbl$label, TRUE, FALSE)

######################################
## parentId
parentId <- edgesJson[grep("oboInOwl#ObsoleteClass",edgesJson$obj, invert = T),c("sub","obj")]
names(parentId) <- c("id","parent")
parentId$id <- gsub("Orphanet","ORPHA",parentId$id)
parentId$parent <- gsub("Orphanet","ORPHA",parentId$parent)
parentId$DB <- gsub(":.*","",parentId$id)
parentId$pDB <- gsub(":.*","",parentId$parent)

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
    sep="\t",
    row.names=FALSE, col.names=TRUE,
    quote=FALSE
  )
}










# owl <- readLines(file.path(sdir,sfi_name))
# 
# ############################
# ## Basic information
# starts <- grep("<owl:Class rdf:about=\"http",owl)
# ends <- c(starts[-1]-1, length(owl))
# ends <- apply(data.frame(starts,ends),1,
#               function(x){
#                 s <- as.numeric(x[1])
#                 e <- grep(".*<rdfs:label rdf",owl[as.numeric(x[1]):as.numeric(x[2])])[1] ## Skipping Axiom class input
#                 if(is.na(e) == TRUE){ e <- x[2]
#                 }else{e <- x[1] + e }
#                 # print(c(x,e))
#                 return(e)
#               })
# efoDef <- do.call(rbind, apply(
#   data.frame(starts, ends),
#   1,
#   function(x){
#     # print(x)
#     termDesc <- trimws(owl[as.numeric(x[1]):as.numeric(x[2])])
#     ## termDesc <- trimws(owl[223354:223486])
#     ## ID
#     fn <- "^<owl:Class rdf:about=\"http"
#     id <- sub("\\\">","", sub(".*/", "", grep(fn, termDesc, value=T)))
#     if(length(id)==0) id <- NA
#     ## label
#     fn <- "<rdfs:label rdf"
#     labels <- sub("</rdfs:label>","",sub(".*\\\">", "", grep(fn, termDesc, value=T)))
#     if(length(labels)==0) labels <- NA
#     ## definitions
#     fn <- "<obo:IAO_0000115 rdf"
#     def <- sub("</obo:IAO_0000115>","",sub(".*\\\">", "", grep(fn, termDesc, value=T)))
#     if(length(def)==0 || def == ""){def <- NA
#     }else{def <- paste(def,collapse = ", ")}
#     ## parent
#     fn <- "<rdfs:subClassOf rdf"
#     parents <- sub("\"/>","",gsub(paste(".*/efo/",".*/obo/",".*/ORDO/",sep = "|"), "", grep(fn, termDesc, value=T)))
#     # fn <- " [!].*$"
#     # parent <- sub(fn, "", parent)
#     if(length(parents)==0) parents <- NA
#     ## cross ids
#     fn <- "^<efo:.*_definition_citation rdf"
#     crossId <- sub(":","_",sub("</efo:.*>","",sub(".*\\\">", "", grep(fn, termDesc, value=T))))
#     if(length(crossId) == 0){crossId <- NA
#     }else{
#       crossId <- paste(unique(crossId), collapse=", ")}
#     ## alternative term
#     fn <- "^<efo:alternative_term rdf"
#     altTerm <- sub("</efo:alternative_term>","",sub(".*\">", "", grep(fn, termDesc, value=T)))
#     if(length(altTerm) == 0){altTerm <- NA
#     }else{
#       altTerm <- paste(unique(altTerm), collapse=", ")}
#     ## Synonym
#     fn <- "^<oboInOwl:hasRelatedSynonym rdf"
#     syn <- sub("</oboInOwl:hasRelatedSynonym>","",sub(".*\\\">", "", grep(fn, termDesc, value=T)))
#     if(length(syn) == 0){syn <- NA
#     }else{syn <- paste(unique(syn), collapse=", ")}
#     # return(def)
#     return(data.frame(
#       id = id, 
#       labels = labels,
#       def = def,
#       parents = parents,
#       crossId = crossId,
#       syn = syn,
#       altTerm = altTerm,
#       stringsAsFactors=F)
#     )
#   }
# ))
# efoDef$id <- gsub("Orphanet","ORPHA",efoDef$id)
# 
# ###############################################################################@
# ## Custom information ----
# ###############################################################################@
# 
# ############################
# ## CrossId
# crossId <- unique(efoDef[, c("id", "crossId")])
# crossIdList <- strsplit(crossId$crossId, ", ")
# names(crossIdList) <- crossId$id
# crossId <- stack(crossIdList)
# colnames(crossId) <- c("id2", "id1")
# crossId$id2 <- as.character(crossId$id2)
# crossId$id1 <- as.character(crossId$id1)
# 
# # crossId <- crossId[grepl(paste("ICD9","ICD10","SNOMEDCT","DOID",
# #                                "UMLS","NCIt","MSH","MESH","OMIM","MeSH","NCIT","SNOMEDCT","NCiT", sep = "|"), crossId$id2),]
# # crossId <- crossId[grepl(paste("Orphanet","EFO","DOID", sep = "|"), crossId$id1),]
# crossId$id2 <- gsub("MESH","MeSH",crossId$id2)
# crossId$id2 <- gsub("MSH","MeSH",crossId$id2)
# crossId$id2 <- gsub("NCIT","NCIt",crossId$id2)
# crossId$id2 <- gsub("NCiT","NCIt",crossId$id2)
# crossId$id2 <- gsub("NCI","NCIt",crossId$id2)
# crossId$id2 <- gsub("NCI Metathesaurus","NCIt",crossId$id2)
# crossId$id2 <- gsub("NCIt Metathesaurus","NCIt",crossId$id2)
# crossId$id2 <- gsub("NCItt","NCIt",crossId$id2)
# crossId$id2 <- gsub("SNOWMEDCT","SNOMEDCT",crossId$id2)
# crossId$id2 <- gsub("SNOMED","SNOMEDCT",crossId$id2)
# crossId$id2 <- gsub("SNOMEDCTCT","SNOMEDCT",crossId$id2)
# crossId$id2 <- gsub("UMLS","MedGen",crossId$id2)
# crossId$id1 <- gsub("Orphanet","ORPHA",crossId$id1)
# crossId$DB1 <- gsub("_.*","",crossId$id1)
# crossId$DB2 <- gsub("_.*","",crossId$id2)
# crossId <- crossId[is.na(crossId$id2) == FALSE, c("DB1","id1","DB2","id2")]
# 
# efoDef <- efoDef[which(efoDef$id %in% crossId$id1),]
# efoDef$DB <- gsub("_.*","",efoDef$id)
# 
# ############################
# ## entryId
# entryId <- efoDef[!duplicated(efoDef$id),c("id","DB")]
# 
# ############################
# ## parentId
# parentId <- unique(efoDef[, c("id", "parents")])
# parentList <- strsplit(parentId$parents, ", ")
# names(parentList) <- parentId$id
# parentId <- stack(parentList)
# colnames(parentId) <- c("parent", "id")
# parentId$id <- as.character(parentId$id)
# parentId$parent <- as.character(parentId$parent)
# parentId$DB <- gsub("_.*","",parentId$id)
# parentId$pDB <- gsub("_.*","",parentId$parent)
# parentId <- parentId[which(parentId$id %in% crossId$id1 & is.na(parentId$parent) == FALSE),
#                      c("DB","id","pDB","parent")]
# 
# ############################
# ## Synonym
# synId <- unique(efoDef[, c("id", "syn")])
# synList <- strsplit(synId$syn, ", ")
# names(synList) <- synId$id
# synId <- stack(synList)
# colnames(synId) <- c("name", "id")
# synId$name <- as.character(synId$name)
# synId$id <- as.character(synId$id)
# synId$DB <- gsub("_.*","",synId$id)
# synId <- synId[which(synId$id %in% crossId$id1 & is.na(synId$name) == FALSE),]
# 
# ############################
# ## Definition and labels
# idDef <- unique(efoDef[, c("DB","id", "labels","def")])
# names(idDef) <- c("DB","id","name","definition")
# idDef$id <- as.character(idDef$id)
# idDef$name <- as.character(idDef$name)
# idDef$definition <- as.character(idDef$definition)
# entryId$definition <- idDef$definition[match(entryId$id,idDef$id)]
# 
# ############################
# ## Names
# idNames <- unique(efoDef[, c("id", "altTerm")])
# idNamesList <- strsplit(idNames$altTerm,", ")
# names(idNamesList) <- idNames$id
# idNames <- stack(idNamesList)
# colnames(idNames) <- c("name", "id")
# idNames$name <- as.character(idNames$name)
# idNames$id <- as.character(idNames$id)
# idNames$DB <- gsub("_.*","",idNames$id)
# 
# idNames <- rbind(idNames[,c("DB","id","name")],
#                  synId[,c("DB","id","name")], 
#                  idDef[,c("DB","id","name")])
# idNames <- idNames[which(idNames$id %in% crossId$id1 & is.na(idNames$name) == FALSE),]
# idNames$canonical <- ifelse(idNames$name %in% idDef$name, TRUE, FALSE)
# 
# ## Ids
# idNames <- as.data.frame(apply(idNames,2,function(x) gsub(".*_","",x)), stringsAsFactors = F)
# parentId <- as.data.frame(apply(parentId,2,function(x) gsub(".*_","",x)), stringsAsFactors = F)
# entryId <- as.data.frame(apply(entryId,2,function(x) gsub(".*_","",x)), stringsAsFactors = F)
# crossId <- as.data.frame(apply(crossId,2,function(x) gsub(".*_","",x)), stringsAsFactors = F)


