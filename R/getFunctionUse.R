#' funsUsedInLine
#'
#' Support function for funsUsedInFile.
#'
#' @param lines (\link[base]{c}) of (\link[base]{character})
#' @param name (\link[base]{character})
#' @param i (\link[base]{numeric})
#' @param verbose (\link[base]{logical}: FALSE)
#'
#' @return (\link[base]{data.frame})
#' | column |              data type |
#' | ------ | ---------------------- |
#' |    pkg | \link[base]{character} |
#' |    fun | \link[base]{character} |
#' |   line |   \link[base]{numeric} |
funsUsedInLine <- function(lines, name, i, verbose = FALSE) {
  line <- lines[i]

  if (!startsWith(line, "#") && !is.na(line) && length(line) >= 0 && line != "NA") {
    line <- paste(
      stringr::str_split(
        string = line,
        pattern = "\\w+\\$",
        simplify = TRUE
      ),
      collapse = ""
    )

    # Remove strings
    line <- stringr::str_replace_all(line, "[\"\'\`].+[\"\'\`]+", "")

    funVec <- unlist(stringr::str_extract_all(
      string = line,
      pattern = "[\\w\\.]+(::)?[\\w\\.]+\\("
    ))

    funVec <- stringr::str_remove_all(
      string = funVec,
      pattern = "\\("
    )

    if ("do.call" %in% funVec) {
      funVec <- append(funVec, getDoCallFromLines(lines))
    }

    if (any(stringr::str_detect(string = funVec, pattern = "[\\w]+?[Aa]pply"))) {
      funVec <- append(funVec, getApplyFromLines(lines))
    }

    if ("plyr::dlply" %in% funVec) {
      funVec <- append(funVec, getDlplyCallFromLines(lines))
    }

    funVec <- stringr::str_split(
      string = funVec,
      pattern = "::"
    )

    if (length(funVec) > 0) {
      funVec <- lapply(
        X = funVec,
        FUN = function(x) {
          if (length(x) == 1) {
            x <- list("unknown", x)
          } else {
            list(x)
          }
        }
      )

      df <- data.frame(t(sapply(funVec, unlist)))
      names(df) <- c("pkg", "fun")

      df <- df %>%
        dplyr::mutate(
          file = name,
          line = i
        ) %>%
        dplyr::tibble()

      return(df)
    } else {
      if (verbose == TRUE) {
        message(paste0("No functions found for line: ", i))
      }
    }
  }
}


#' funsUsedInFile
#'
#' Support function
#'
#' @param files (\link[base]{list}) of (\link[PaRe]{File})
#' @param verbose (\link[base]{logical})
#'
#' @return (\link[base]{list})
funsUsedInFile <- function(files, verbose = FALSE) {
  lapply(X = files, FUN = function(file) {
    if (verbose) {
      message(paste0("Started on file: ", file$getName()))
    }

    lines <- file$getLines()

    out <- lapply(
      X = seq_len(length(lines)),
      FUN = function(i) {
        funsUsedInLine(lines = file$getLines(), name = file$getName(), i = i)
      }
    )
  })
}

#' summariseFunctionUse
#'
#' Summarise functions used in R package.
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#' @param verbose (\link[base]{logical}: FALSE)\cr
#' Prints message to console which file is currently being worked on.
#'
#' @return (\link[dplyr]{tibble})
#' | column |              data type |
#' | ------ | ---------------------- |
#' |   file | \link[base]{character} |
#' |   line |   \link[base]{numeric} |
#' |    pkg | \link[base]{character} |
#' |    fun | \link[base]{character} |
#'
#' @examples
#' fetchedRepo <- tryCatch(
#'   {
#'     # Set dir to clone repository to.
#'     tempDir <- tempdir()
#'     pathToRepo <- file.path(tempDir, "glue")
#'
#'     # Clone repo
#'     git2r::clone(
#'       url = "https://github.com/tidyverse/glue.git",
#'       local_path = pathToRepo
#'     )
#'
#'     # Create instance of Repository object.
#'     repo <- PaRe::Repository$new(path = pathToRepo)
#'
#'     # Set fetchedRepo to TRUE if all goes well.
#'     TRUE
#'   },
#'   error = function(e) {
#'     # Set fetchedRepo to FALSE if an error is encountered.
#'     FALSE
#'   },
#'   warning = function(w) {
#'     # Set fetchedRepo to FALSE if a warning is encountered.
#'     FALSE
#'   }
#' )
#'
#' if (fetchedRepo) {
#'   # Run getFunctionUse on the Repository object.
#'   getFunctionUse(repo = repo, verbose = TRUE)
#' }
getFunctionUse <- function(repo, verbose = FALSE) {
  files <- repo$getRFiles()

  funUse <- funsUsedInFile(files, verbose)

  if (length(funUse) == 0) {
    warning("No functions found, output will be empty")
    funUse <- dplyr::tibble(
      file = character(0),
      line = numeric(0),
      pkg = character(0),
      fun = character(0)
    )
  }

  funUse <- dplyr::bind_rows(funUse) %>%
    dplyr::relocate("file", "line", "pkg", "fun") %>%
    dplyr::arrange(.data$file, .data$line, .data$pkg, .data$fun)

  funUse$pkg[funUse$fun %in% ls("package:base")] <- "base"
  return(funUse)
}
