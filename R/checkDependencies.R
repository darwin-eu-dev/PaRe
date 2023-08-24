#' printMessage
#'
#' Prints messages dependening of the nrow of the number of rows of the
#' notPermitted and versionCheck data.frames
#'
#' @param notPermitted ([base]{data.frame})
#' @param versionCheck ([base]{data.frame})
#'
#' @return (\link[base]{data.frame})
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | \link[base]{character} |
#' | version | \link[base]{character} |
printMessage <- function(notPermitted, versionCheck) {
  if (nrow(notPermitted) > 0) {
    message(paste0(
        sprintf("The following are not permitted: {%s}\n", paste0(notPermitted$package, collapse = ", ")),
        "Please open an issue here: 'https://github.com/mvankessel-EMC/DependencyReviewerWhitelists/issues'"
      )
    )
    return(notPermitted)
  } else if (nrow(versionCheck) > 0) {
    message(paste0(
      sprintf("The following versions are not of the right version: %s\n", paste0(versionCheck$package, collapse = ", ")),
      "Please open an issue here: 'https://github.com/mvankessel-EMC/DependencyReviewerWhitelists/issues'"
    ))
    return(versionCheck)
  } else {
    message("All dependencies are approved.")
    return(NULL)
  }
}

#' getVersionDf
#'
#' Function to compare different versions.
#'
#' @param dependencies (\link[base]{data.frame})
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | \link[base]{character} |
#' | version | \link[base]{character} |
#' @param permittedPackages (\link[base]{data.frame})
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | \link[base]{character} |
#' | version | \link[base]{character} |
#'
#' @return (\link[base]{data.frame})
#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | \link[base]{character} |
#' | version | \link[base]{character} |
getVersionDf <- function(dependencies, permittedPackages) {
  # permitted <- dependencies %>%
  #   dplyr::filter(.data$package %in% permittedPackages$package)

  permitted <- dependencies[
    i = dependencies$package %in% permittedPackages$package
  ]

  permitted$version[permitted$version == "*"] <- "0.0.0"

  permittedPackages <- permittedPackages[
    i = permittedPackages$package %in% permitted$package][
      i = order(rank(dependencies$package))
    ]

  dt <- cbind(
    permittedPackages,
    allowed = permitted$version
  )

  return(dt[
    i = !as.numeric_version(dt$version) >= as.numeric_version(dt$allowed)
  ])
}

#' checkDependencies
#'
#' Check package dependencies
#'
#' @export
#'
#' @param repo (\link[PaRe]{Repository})\cr
#' Repository object.
#' @param dependencyType (\link[base]{character})\cr
#' Types of dependencies to be included
#' @param verbose (\link[base]{logical}: TRUE)
#' TRUE or FALSE. If TRUE, progress will be reported.
#'
#' @return (\link[base]{data.frame})\cr
#' Data frame with all the packages that are now permitted.

#' |  column |              data type |
#' | ------- | ---------------------- |
#' | package | \link[base]{character} |
#' | version | \link[base]{character} |
#'
#' @examples
#' donttest {
#' # Set cahce, usually not required.
#' withr::local_envvar(
#'   R_USER_CACHE_DIR = tempfile()
#' )
#'
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
#'   # Use checkDependencies on the Repository object.
#'   checkDependencies(repo)
#'   checkDependencies(repo, dependencyType = c("Imports", "Suggests"))
#' }
#' }
checkDependencies <- function(
    repo,
    dependencyType = c("Imports", "Depends"),
    verbose = TRUE) {
  description <- repo$getDescription()

  deps <- description$get_deps() |>
    data.table::data.table()

  dependencies <- deps[
    i = deps$type %in% dependencyType & deps$package != "R",
    j = .(package, version)
  ]

  dependencies$version <- stringr::str_remove(
    string = dependencies$version,
    pattern = "[\\s>=<]+"
  )

  if (isTRUE(verbose)) {
    permittedPackages <- getDefaultPermittedPackages() |>
      data.table::data.table()

    permittedPackages[
      , version := as.character.numeric_version(permittedPackages$version)
    ]
  } else {
    suppressMessages(
      permittedPackages <- getDefaultPermittedPackages() |>
        data.table::data.table()
    )

    permittedPackages[
      , version := as.character.numeric_version(permittedPackages$version)
    ]
  }

  notPermitted <- dependencies[
    i = !dependencies$package %in% permittedPackages$package
  ]

  permitted <- dependencies[
    i = dependencies$package %in% permittedPackages$package
  ]

  permitted$version[permitted$version == "*"] <- "0.0.0"

  return(printMessage(
    notPermitted,
    getVersionDf(dependencies, permittedPackages)
  ))
}
