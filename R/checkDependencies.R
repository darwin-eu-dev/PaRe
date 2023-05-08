#' printMessage
#'
#' Prints messages dependening of the nrow of the number of rows of the
#' notPermitted and versionCheck data.frames
#'
#' @param notPermitted
#' <[base]{data.frame}> notPermitted
#' @param versionCheck
#' <[base]{data.frame}> versionCheck
#'
#' @return
#' <\link[base]{data.frame}> or <\link[base]{NULL}>
printMessage <- function(notPermitted, versionCheck) {
  if (nrow(notPermitted) > 0) {
    cli::cli_alert(glue::glue(
      cli::col_red("The following are not permitted: {cli::style_bold(paste0(notPermitted$package, collapse = ', '))}")))
    cli::cli_alert(glue::glue(
      "Please open an issue here: {cli::style_bold('https://github.com/mvankessel-EMC/DependencyReviewerWhitelists/issues')}"
    ))
    return(notPermitted)
  } else if (nrow(versionCheck) > 0) {
    cli::cli_alert(glue::glue(
      "The following versions are not of the right version: {cli::col_yellow(paste0(versionCheck$package, collapse = ', '))}"))
    cli::cli_alert(glue::glue(
      "Please open an issue here: {cli::style_bold('https://github.com/mvankessel-EMC/DependencyReviewerWhitelists/issues')}"
    ))
    return(versionCheck)
  } else {
    cli::cli_alert(cli::col_green(cli::style_bold(
      "All dependencies are approved.")))
    return(NULL)
  }
}

#' getVersionDf
#'
#' Function to compare different versions.
#'
#' @param dependencies
#' <\link[base]{data.frame}> All dependencies of he package.
#' @param permittedPackages
#' <\link[base]{data.frame}> of all permitted packages.
#'
#' @return
#' <\link[base]{data.frame}> with all non permitted packages based on version.
getVersionDf <- function(dependencies, permittedPackages) {
  permitted <- dependencies %>%
    dplyr::filter(.data$package %in% permittedPackages$package)

  permitted$version[permitted$version == "*"] <- "0.0.0"

  permitted <- permitted %>%
    dplyr::arrange(.data$package)

  permittedPackages <- permittedPackages[
    permittedPackages$package %in% permitted$package, ] %>%
    dplyr::arrange(.data$package)

  df <- cbind(
    permittedPackages,
    allowed = permitted$version)

  return(df[
    !as.numeric_version(df$version) >= as.numeric_version(df$allowed), ])
}

#' checkDependencies
#'
#' Check package dependencies
#'
#' @param repo
#' <\link[PaRe]{Repository}> object.
#' @param dependencyType
#' <\link[base]{character}> Types of dependencies to be included
#' @param verbose
#' <\link[base]{logical}> TRUE or FALSE. If TRUE, progress will be reported.
#'
#' @return
#' <\link[base]{data.frame}> with all the packages that are now permitted.
#' @export
checkDependencies <- function(
    repo,
    dependencyType = c("Imports", "Depends"),
    verbose = TRUE) {
  description <- repo$getDescription()

  dependencies <- description$get_deps() %>%
    dplyr::filter(.data$type %in% dependencyType) %>%
    dplyr::select("package", "version")

  dependencies <- dependencies %>%
    dplyr::filter(.data$package != "R")

  dependencies$version <- stringr::str_remove(
    string = dependencies$version,
    pattern = "[\\s>=<]+")

  if (isTRUE(verbose)){
    permittedPackages <- getDefaultPermittedPackages()
  } else {
    suppressMessages(
      permittedPackages <- getDefaultPermittedPackages()
    )
  }

  notPermitted <- dependencies %>%
    dplyr::filter(!.data$package %in% permittedPackages$package)

  permitted <- dependencies %>%
    dplyr::filter(.data$package %in% permittedPackages$package)

  permitted$version[permitted$version == "*"] <- "0.0.0"

  return(printMessage(
    notPermitted,
    getVersionDf(dependencies, permittedPackages)))
}
