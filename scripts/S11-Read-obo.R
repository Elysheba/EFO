# rm(list=ls())
# gc()

# of <- "../sources/efo/efo_full.obo"

getOboInfo <- function(of){
   obo <- readLines(of)
   ## * Basic information ----
   starts <- which(obo=="[Term]" | obo=="[Typedef]")
   ends <- c(starts[-1]-1, length(obo))
   efoDef <- apply(
      data.frame(starts, ends),
      1,
      function(x){
         class <- obo[x[1]]
         termDesc <- obo[(x[1]+1):(x[2]-1)]
         ##
         fn <- "^id: "
         id <- sub(fn, "", grep(fn, termDesc, value=T))
         if(length(id)==0) id <- NA
         ##
         fn <- "^name: "
         name <- sub(fn, "", grep(fn, termDesc, value=T))
         if(length(name)==0) name <- NA
         ##
         fn <- "^def: "
         def <- sub(fn, "", grep(fn, termDesc, value=T))
         if(length(def)==0) def <- NA
         ##
         fn <- "^is_a: "
         parent <- sub(fn, "", grep(fn, termDesc, value=T))
         fn <- " [!].*$"
         parent <- sub(fn, "", parent)
         if(length(parent)==0) parent <- NA
         ##
         # fn <- "^alt_id: "
         # altId <- sub(fn, "", grep(fn, termDesc, value=T))
         # altId <- paste(unique(c(id, altId)), collapse=", ")
         ##
         return(list(
            class=class,
            id=id, name=name, def=def,
            parent=parent)
            # altId=altId,
            # stringsAsFactors=F)
         )
      }
   )
   efoDef.summary <- lapply(efoDef, function(x) unlist(lapply(x, length)))
   efoDef.summary <- do.call(rbind, efoDef.summary)
   apply(efoDef.summary, 2, max)
   
   efoTypedef <- efoDef[which(
      unlist(lapply(efoDef, function(x) x$class=="[Typedef]"))
   )]
   efoTerms <- efoDef[which(
      unlist(lapply(efoDef, function(x) x$class=="[Term]"))
   )]
   
   termNames <- do.call(rbind, lapply(
      efoTerms,
      function(x){
         data.frame(
            id=x$id,
            name=x$name,
            stringsAsFactors=FALSE
         )
      }
   ))
   termDef <- do.call(rbind, lapply(
      efoTerms,
      function(x){
         data.frame(
            id=x$id,
            def=x$def,
            stringsAsFactors=FALSE
         )
      }
   ))
   termParents <- do.call(rbind, lapply(
      efoTerms,
      function(x){
         data.frame(
            id=x$id,
            parent=x$parent,
            stringsAsFactors=FALSE
         )
      }
   ))
   
   return(list(
      termNames=termNames,
      termDef=termDef,
      termParents=termParents
   ))
}
