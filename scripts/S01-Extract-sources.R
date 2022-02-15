# setwd("~/Shared/Data-Science/Data-Source-Model-Repository/EFO/scripts/")

library(git2r)
library(RJSONIO)
library(here)
source(here("../00-Utils/downloadSourceFiles.R"))

desc <- readJSONStream(here("DESCRIPTION.json"))

sourceFiles <- desc$"source files"
urls <- unlist(lapply(
   sourceFiles,
   function(sf){
      toRet <- sf$"URL template"
      names(toRet) <- sf$"name"
      return(toRet)
   }
))
srcDir <- here("sources")

downloadSourceFiles(urls, srcDir, httpForce = TRUE)

# gitRepo <- dirname(urls)
# gitRepo <- "https://github.com/EBISPOT/efo/"
# 
# ## Clone or pull git repository
# if(!dir.exists(srcDir)){
  # gitRepo <- git2r::clone(url = gitRepo, local_path = srcDir)
# }else{
#   gitRepo <- git2r::repository(srcDir)
#   git2r::pull(gitRepo)
# }
# # 
# # ###############################################
# # ## Information source files
# rcurrent <- git2r::odb_blobs(gitRepo)
# rcurrent <- tail(rcurrent[rcurrent$name == "efo.owl",], n = 1L)
# 
# EFO_sourceFiles <- data.frame(url = urls[[1]],
#                               current = rcurrent$when)

# ###############################################
# ## Writing files
# toSave <- grep("^EFO[_]", ls(), value = T)
# ddir <- "../data"
# 
# write.table(get(toSave), row.names = FALSE, sep = "\t", quote = FALSE, file=file.path(ddir, paste(toSave, ".txt", sep="")))
# 
# write.table(EFO_sourceFiles, file=file.path(ddir, paste(toSave, ".txt", sep="")))
