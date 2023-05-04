#' @title
#' R6 Repository class.
#' @description
#' Class representing the Repository
#' @export
Repository <- R6::R6Class(
  classname = "Repository",
  # Public ----
  public = list(
    #' @description
    #' Initializer for Repository class
    #'
    #' @param path
    #' Path to R package project
    initialize = function(path) {
      private$path <- normalizePath(path)
      private$name <- basename(private$path)
      private$git <- git2r::in_repository(private$path)
      private$description <- desc::description$new(private$path)
      private$validate()

      private$fetchRFiles()
      return(invisible(self))
    },

    #' @description
    #' Get method for name.
    #'
    #' @return (`character()`)\cr
    #' Repository name
    getName = function() {
      return(private$name)
    },

    #' @description
    #' Get method fro path
    #'
    #' @return (`character()`)\cr
    #' Path to Repository folder
    getPath = function() {
      return(private$path)
    },

    #' @description
    #' Get method to get a list of (`File`) objects.
    #'
    #' @return (`list()`)
    #' List of File objects
    getFiles = function() {
      return(private$rFiles)
    },

    #' @description
    #' Get method to get the description of the package.
    #' See: \link[desc]{description}.
    #'
    #' @return (`desc::description`)\cr
    #' Description object
    getDescription = function() {
      return(private$description)
    },

    #' @description
    #' Get method for functionUse, will check if functionUse has already been
    #' fetched or not.
    #'
    #' @return (`data.frame()`)
    #' data.frame containing function use.
    getFunctionUse = function() {
      if (is.null(private$funtionUse)) {
        message("functionUse not yet fetched.")
        input <- readline(
          "Would you like to fetch now? (y/n)")
        if (tolower(input) == "y") {
          private$functionUse <- getFunctionUse(private$rFiles, verbose = TRUE)
          return(private$functionUse)
        } else {
          message("You can use the `fetchFunctionUse()` method aswell.")
        }
      } else {
        return(private$functionUse)
      }
    },

    #' @description
    #' Fetch functionUse data.frame.
    #'
    #' @param ...
    #' Further parameters for \link[PaRe]{getFunctionUse}
    #'
    #' @return (`invisible(self`)
    fetchFunctionUse = function(...) {
      private$functionUse <- getFunctionUse(private$rFiles, ...)
      return(invisible(self))
    },

    #' @description
    #' Check if used dependencies are in accordance with the specified PaRe
    #' white list.
    #'
    #' @return (`invisible(self)`)
    checkDependencies = function() {
      R6checkDependencies(self)
      return(invisible(self))
    },

    #' @description
    #' Counts lines per type of file.
    #'
    #' @param ...
    #' Further parameters for \link[PaRe]{countPackageLines}
    #'
    #' @return (`data.frame()`)\cr
    #' data.frame containing the amount of lines per file type.
    linesPerType = function(...) {
      return(countPackageLines(self, ...))
    },

    # #' @description
    # #' Method to run 'git blame' on package files matched by a regex pattern.
    # #'
    # #' @param ... Parameters for \link[PaRe]{gitBlameRepo}
    # #'
    # #' @return (`data.frame()`)
    # gitBlame = function(...) {
    #   PaRe::gitBlameRepo(repoPath = private$path, ...)
    #},

    #' @description
    #' Method to run 'git checkout <branch/commit hash>'
    #'
    #' @param branch (`character()`)\cr
    #' Name of branch or a hash referencing a specific commit.
    #' @param ...
    #' Further parameters for \link[git2r]{checkout}
    #'
    #' @return (`invisible(self)`)
    gitCheckout = function(branch, ...) {
      tryCatch({
        git2r::checkout(object = private$path, branch = branch, ...)
        message(glue::glue("Switched to: {branch}"))
        message("Re-initializing")
        self$initialize(path = private$path)
      }, error = function(e) {
        message(glue::glue("Availible branches: {paste(names(git2r::branches(private$path)), collapse = ', ')}"))
        stop(glue::glue("Branches: '{branch}' not found"))
      })
      return(invisible(self))
    },

    #' @description
    #' Method to run 'git pull'
    #'
    #' @param ...
    #' Further parameters for \link[git2r]{pull}
    #'
    #' @return (`invisible(self)`)
    gitPull = function(...) {
      message("Pulling latest")
      git2r::pull(repo = private$path, ...)
      message("Re-initializing")
      self$initialize(path = private$path)
      return(invisible(self))
    }
  ),
  # Private ----
  private = list(
    name = "name",
    path = "",
    rFiles = NULL,
    git = NULL,
    description = NULL,
    functionUse = NULL,

    validate = function() {
      errorMessages <- checkmate::makeAssertCollection()
      # .rproj file
      rproj <- list.files(file.path(private$path), pattern = "*.Rproj", full.names = TRUE)
      if (length(rproj) == 0) {
        rproj <- ".Rproj"
      }
      checkmate::assertFileExists(rproj, add = errorMessages)
      checkmate::reportAssertions(collection = errorMessages)

      status <- git2r::status(repo = private$path)
      if (length(status$staged) > 0) {
        warning(glue::glue("Staged chagned not committed, unexpected behaviour expected."))
      }
      return(invisible(self))
    },

    fetchRFiles = function() {
      paths <- list.files(file.path(private$path, "R"), full.names = TRUE)

      private$rFiles <- unlist(lapply(paths, function(path) {
        File$new(path = path)
      }))
      return(invisible(self))
    }
  )
)
