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
    #' <\link[base]{character}> Path to R package project
    #'
    #' @return
    #' `invisible(self)`
    initialize = function(path) {
      private$path <- normalizePath(path)
      private$name <- basename(private$path)
      private$git <- git2r::in_repository(private$path)
      private$description <- desc::description$new(private$path)
      private$functionUse <- NULL
      private$validate()

      private$fetchRFiles()
      private$fetchCppFiles()
      private$fetchJavaFiles()
      private$fetchSqlFiles()
      return(invisible(self))
    },

    #' @description
    #' Get method for name.
    #'
    #' @return
    #' <\link[base]{character}> Repository name
    getName = function() {
      return(private$name)
    },

    #' @description
    #' Get method fro path
    #'
    #' @return
    #' <\link[base]{character}> Path to Repository folder
    getPath = function() {
      return(private$path)
    },

    #' @description
    #' Get method to get a list of \link[PaRe]{File} objects.
    #'
    #' @return
    #' <\link[base]{list}> List of File objects
    getFiles = function() {
      files <- list(
        R = private$rFiles,
        cpp = private$cppFiles,
        o = private$oFiles,
        h = private$hFiles,
        java = private$javaFiles,
        sql = private$sqlFiles
      )
      return(files)
    },

    #' @description
    #' Get method to get only R-files.
    #'
    #' @return
    #' <\link[base]{character}> of <\link[PaRe]{File}> objects.
    getRFiles = function() {
      return(private$rFiles)
    },

    #' @description
    #' Get method to get the description of the package.
    #' See: \link[desc]{description}.
    #'
    #' @return
    #' <\link[desc]{description}> Description object.
    getDescription = function() {
      return(private$description)
    },

    ##' @description
    #' Get method for functionUse, will check if functionUse has already been
    #' fetched or not.
    #'
    #' @return
    #' <\link[base]{data.frame}> data.frame containing function use.
    getFunctionUse = function() {
      return(private$functionUse)
    },

    #' @description
    #' Method to run 'git checkout <branch/commit hash>'
    #'
    #' @param branch
    #' <\link[base]{character}> Name of branch or a hash referencing a specific
    #' commit.
    #' @param ...
    #' Further parameters for \link[git2r]{checkout}.
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
    #' Further parameters for \link[git2r]{pull}.
    #'
    #' @return
    #' `invisible(self)`
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
    cppFiles = NULL,
    oFiles = NULL,
    hFiles = NULL,
    sqlFiles = NULL,
    javaFiles = NULL,
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
      paths <- list.files(file.path(private$path, "R"), full.names = FALSE, recursive = TRUE)

      private$rFiles <- unlist(lapply(paths, function(path) {
        File$new(repoPath = private$path, filePath = file.path("R", path))
      }))
      return(invisible(self))
    },

    fetchCppFiles = function() {
      paths <- list.files(file.path(private$path, "src"), full.names = TRUE, recursive = TRUE)

      cpp <- paths[endsWith(paths, ".cpp")]
      o <- paths[endsWith(paths, ".o")]
      h <- paths[endsWith(paths, ".h")]

      private$cppFiles <- lapply(cpp, function(path) {
        File$new(path = path)
      })

      private$oFiles <- lapply(o, function(path) {
        File$new(path = path)
      })

      private$hFiles <- lapply(h, function(path) {
        File$new(path = path)
      })
    },

    fetchJavaFiles = function() {
      paths <- list.files(file.path(private$path, "java"), full.names = TRUE, recursive = TRUE)
      paths <- paths[endsWith(paths, ".java")]

      private$javaFiles <- lapply(paths, function(path) {
        File$new(path = path)
      })
    },

    fetchSqlFiles = function() {
      paths <- list.files(file.path(private$path, "sql"), full.names = TRUE, recursive = TRUE)
      paths <- append(paths, list.files(file.path(private$path, "inst", "sql"), full.names = TRUE, recursive = TRUE))
      paths <- paths[endsWith(paths, ".sql")]

      private$sqlFiles <- lapply(paths, function(path) {
        File$new(path = path)
      })
    }
  )
)
