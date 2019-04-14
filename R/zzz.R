.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to colorbuildr")
}

.onLoad <- function(libname, pkgname) {
  op <- options()

  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "Your name goes here",
    devtools.desc.author = "Jake Riley <rjake@sas.upenn.edu> [aut, cre]",
    devtools.desc.license = "GNU GENERAL PUBLIC LICENSE",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )

  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])

  invisible()
}
