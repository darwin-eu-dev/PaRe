#' @title
#' R6 File class
#'
#' @description
#' Class representing a file containing code.
File <- R6::R6Class(
  classname = "File",
  inherit = Code,
  # Public ----
  public = list(
    #' @description
    #' Initializer method
    #'
    #' @param repoPath
    #' <\link[base]{character}> Path to repository.
    #' @param filePath
    #' <\link[base]{character}> Relative path to file
    #' @return
    #' `invisible(self)`
    initialize = function(repoPath, filePath) {
      private$repoPath <- repoPath
      private$filePath <- filePath
      private$name <- basename(filePath)
      private$type <- stringr::str_split_i(string = private$name, pattern = "\\.", i = 2)
      private$comment <- private$commentSwitch()
      private$lines <- readLines(file.path(repoPath, filePath))

      super$initialize(private$name, private$lines)

      if (private$type == "R") {
        private$fetchDefinedFunctions()
      }

      private$gitBlame()
      return(invisible(self))
    },

    #' @description
    #' Get method to get a list of Function objects
    #'
    #' @return
    #' <\link[base]{list}> of \link[PaRe]{Function} objects.
    getFunctions = function() {
      return(private$functions)
    },

    #' @description
    #' Get method to retrieve the function table.
    #'
    #' @return
    #' <\link[base]{data.frame}>
    getFunctionTable = function() {
      return(private$functionTable)
    },

    #' @description
    #' Gets type of file
    #'
    #' @return
    #' <\link[base]{character}>
    getType = function() {
      return(private$type)
    },

    #' @description
    #' Gets relative file path
    #'
    #' @return
    #' <\link[base]{character}>
    getFilePath = function() {
      return(private$filePath)
    },

    #' @description
    #' Gets table of git blame
    #'
    #' @return
    #' <\link[dplyr]{tibble}>
    getBlameTable = function() {
      return(private$blameTable)
    }
  ),
  # Private ----
  private = list(
    repoPath = "",
    filePath = "",
    type = "",
    functions = NULL,
    comment = "",
    fileFunctions = NULL,
    functionTable = NULL,
    blameTable = NULL,

    validate = function() {
      path <- normalizePath(file.path(private$repoPath, private$filePath))

      errorMessages <- checkmate::makeAssertCollection()
      checkmate::assertFileExists(path)
      checkmate::reportAssertions(collection = errorMessages)
      return(invisible(self))
    },

    gitBlame = function() {
      b <- git2r::blame(repo = private$repoPath, path = private$filePath)
      private$blameTable <- lapply(b$hunks, function(hunk) {
        data.frame(
          author = hunk$orig_signature$name,
          file = basename(hunk$orig_path)
        )
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::tibble()
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

          # Create Function object
          funObj <- Function$new(
            name = funNames[i],
            lineStart = fun$constructorStart,
            lineEnd = fun$bodyEnd,
            lines = private$lines[fun$constructorStart:fun$bodyEnd]
          )

          # Update functionTable
          private$functionTable <- dplyr::bind_rows(
            private$functionTable,
            funObj$getFunction()
          )

          return(funObj)
        })
      return(invisible(self))
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
    },

    commentSwitch = function() {
      return(
        switch(
          EXPR = private$type,
          R = c("#"),
          cpp = c("//"),
          java = c("//"),
          sql = c("#")
        )
      )
    }
  )
)
