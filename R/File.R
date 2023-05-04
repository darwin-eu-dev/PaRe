#' @description
#' R6 <File> class representing a file containging code.
File <- R6::R6Class(
  classname = "File",
  inherit = Code,
  public = list(
    initialize = function(path) {
      private$path <- path
      private$name <- basename(path)
      private$lines <- readLines(path)

      super$initialize(private$name, private$lines)

      private$fetchDefinedFunctions()
      return(invisible(self))
    },

    getFunctions = function() {
      return(private$functions)
    }
  ),
  private = list(
    path = "",
    functions = NULL,

    validate = function() {
      path <- normalizePath(private$path)

      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertFileExists(private$path)
      checkmate::reportAssertions(collection = errorMessages)
      return(invisible(self))
    },

    fetchDefinedFunctions = function() {
      funStart <- grep(
        pattern = "\\w+[ ]?<\\-[ ]?function\\(",
        x = private$lines)

      funConstructor <- private$lines[funStart]
      funNames <- stringr::str_extract(string = funConstructor, pattern = "[\\w\\d\\.]+")

      private$functions <- lapply(X = seq_len(length(funStart)), FUN = function(i) {
          fun <- private$getBodyIndices(line = funStart[i])
          Function$new(
            name = funNames[i],
            lineStart = fun$constructorStart,
            lineEnd = fun$bodyEnd,
            lines = private$lines[fun$constructorStart:fun$bodyEnd]
          )
        })
    },

    getBetween = function(line, patOpen, patClosed) {
      stop <- FALSE
      lineEnd <- line

      cntOpen <- 0
      cntClosed <- 0

      while (!stop) {
        cntOpen <- cntOpen + stringr::str_count(string = private$lines[lineEnd], patOpen)
        cntClosed <- cntClosed + stringr::str_count(string = private$lines[lineEnd], patClosed)

        if (cntOpen == cntClosed & length(cntOpen) > 0 | is.na(private$lines[lineEnd])) {
          stop <- TRUE
        } else {
          lineEnd <- lineEnd + 1
        }
      }
      return(data.frame(
        start = line,
        end = lineEnd
      ))
    },

    getBodyIndices = function(line) {
      # Parameters
      switchOff <- TRUE
      # Get start of body
      constructor <- private$getBetween(
        line = line,
        patOpen = "\\(",
        patClosed = "\\)")

      body <- private$getBetween(
        line = constructor$end,
        patOpen = "\\{",
        patClosed = "\\}")

      return(data.frame(
        constructorStart = constructor$start,
        constructorEnd = constructor$end,
        bodyStart = body$start,
        bodyEnd = body$end
      ))
    },

    goToBody = function(line) {
      startFun <- FALSE
      bodyLine <- line

      bracOpen <- 0
      bracClosed <- 0

      while (!startFun) {
        bracOpen <- bracOpen + stringr::str_count(string = private$lines[bodyLine], "\\(")
        bracClosed <- bracClosed + stringr::str_count(string = private$lines[bodyLine], "\\)")

        if (bracOpen == bracClosed & bracOpen > 0) {
          startFun <- TRUE
        } else {
          bodyLine <- bodyLine + 1
        }
      }
      return(bodyLine)
    }
  )
)
