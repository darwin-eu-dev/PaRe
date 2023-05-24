CallInvestigator <- R6::R6Class(
  classname = "CallInvestigator",
  public = list(
    initialize = function() {
      return(invisible(self))
    },

    validate = function() {
      return(invisible(self))
    },

    getCalls = function(lines) {
      out <- private$getCallsFromLine(lines)
      funs <- out$funs
      indices <- out$indices
      # print(length(funs))
      # print(length(indices))
      if (length(funs) > 0) {
        funVec <- lapply(
          X = funs,
          FUN = function(fun) {
            if (length(fun) == 1) {
              x <- list("unknown", fun)
            } else {
              list(fun)
            }
          })

        df <- data.frame(t(sapply(funVec, unlist)))
        names(df) <- c("pkg", "fun")
        # print(nrow(df))
        #df$line <- indices
        return(df)
      } else {
        return(NULL)
      }
    }
  ),
  private = list(
    pattern = "([\\w\\.]+(::))?\\w+\\(",

    getCallsFromLine = function(lines) {
      indices <- grep(pattern = private$pattern, x = lines)

      funs <- lines[indices] %>%
        stringr::str_extract_all(pattern = private$pattern) %>%
        unlist() %>%
        stringr::str_remove_all(pattern = "\\(") %>%
        stringr::str_split(pattern = "::")

      return(list(funs = funs, indices = indices))
    },

    getMultiLineFun = function(line, lines) {
      nLine <- line

      # Init
      doCallVec <- c()
      bracOpen <- 0
      bracClose <- 0

      while (bracOpen != bracClose || bracOpen < 1 && bracClose < 1) {
        if (!is.na(lines[nLine])) {
          bracOpen <- bracOpen + stringr::str_count(string = lines[nLine], pattern = "\\(")
          bracClose <- bracClose + stringr::str_count(string = lines[nLine], pattern = "\\)")

          doCallVec <- append(doCallVec, lines[nLine])
        }
        nLine <- nLine + 1

        if (nLine > length(lines)) {
          break
        }
      }
      return(doCallVec)
    }
  )
)
callInv <- CallInvestigator$new()
b <- callInv$getCalls(lines)
