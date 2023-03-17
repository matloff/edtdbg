
ksInit
function () 
{
    lst <<- list(f = function() NA, prnt = TRUE)
    class(lst) <<- "ksr"
    ksProto <<- lst
    print.ksr <<- function(ksobj) {
        if (ksobj$prnt) 
            print(ksobj$f())
        else ksobj$f()
    }
    gtd <<- ksProto
    gtd$f <<- getwd
    ksList <<- data.frame(opName = "gtd", op = "getwd()")
    upd <<- ksProto
    upd$f <<- function() {
        setwd("..")
        message(getwd())
    }
    upd$prnt <<- FALSE
    ksList <<- rbind(ksList, data.frame(opName = "upd", op = "setwd(..)"))
    sstd <<- ksProto
    sstd$f <<- function() {
        d <- readline("new directory: ")
        saveDir <<- getwd()
        setwd(d)
        print(getwd())
    }
    ksList <<- rbind(ksList, data.frame(opName = "sstd", op = "save dir, setwd()"))
    bkd <<- ksProto
    bkd$f <<- function() {
        setwd(saveDir)
        saveDir <<- NULL
        message(getwd())
    }
    ksList <<- rbind(ksList, data.frame(opName = "bkd", op = "back to saved dir"))
}

ksAbbrev <- function (name, op, hasArgs) 
{
    cmd <- paste0(name, " <<- ksProto")
    evalrstring(cmd)
    cmd <- paste0(name, "$f <<- function() ", op)
    evalrstring(cmd)
    ksList <<- rbind(ksList, data.frame(opName = name, op = op))
}

