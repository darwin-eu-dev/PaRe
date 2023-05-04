#' @description
#' R6 <Repository> class representing the Repository
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

    getName = function() {
      return(private$name)
    },

    getPath = function() {
      return(private$path)
    },

    getFiles = function() {
      return(private$rFiles)
    },

    getDescription = function() {
      return(private$description)
    },

    getFunctionUse = function(...) {
      getFunctionUse(private$rFiles, ...)
    },

    checkDependencies = function() {
      R6checkDependencies(self)
    },

    linesPerType = function(...) {
      R6countPackageLines(self, ...)
    },

    gitBlame = function(...) {
      PaRe::gitBlameRepo(repoPath = private$path, ...)
    },

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
    },

    gitPull = function(...) {
      message("Pulling latest")
      git2r::pull(repo = private$path, ...)
      message("Re-initializing")
      self$initialize(path = private$path)
    }
  ),
  # Private ----
  private = list(
    name = "name",
    path = "",
    rFiles = NULL,
    git = NULL,
    description = NULL,

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
