#' @export
rm.all <-
  function() {
    rm(list = ls(all = TRUE))
  }

#TODO: Implement parameters and pattern matching
#' @export
ls.pkgs <- function(all = F, version = F) {
  versions <- installed.packages()[, "Version"]
  loaded <- names(sessionInfo()$otherPkgs)
  if (all & !version) return(names(versions))
  if (all & version) return(versions)
  if (!all & !version) return(loaded)
  if (!all & version) return(versions[names(versions) %in% loaded])
}

#TODO: Implement parameters and pattern matching
#TODO: Enter a list of name spaces (and not a vector of strings. namespaces as parameters, just like in rm)
#' @export
detach.packages <- function(packages, force = F) {
  uld.errs <- c()

    for (pkg in paste("package", packages, sep = ":")) {
      uld.result <- tryCatch(
        detach(
          pkg,
          character.only = TRUE,
          unload = TRUE,
          force = force
        )
        ,
        warning = function(w) {
        },
        error = function(e) {
          return(pkg)
        }
      )
      uld.errs <- c(uld.result, uld.errs)

    }
    if (length(uld.errs) > 0) {
      warning(paste(
        "There were issues unloading the following package(s):",
        paste(uld.errs, collapse = ", ")
      ),
      immediate. = T)
    }
  }

#' @export
detach.all <- function()
{
  if (!is.null(ls.pkgs())) {
    detach.packages(ls.pkgs())
  }
}

#TODO separate the installation script so that we can call both cran and github using the same function
#' @export
attach.packages <-
  function(packages, init = F, messages = F) {
    inst.errs <- c()
    ld.errs <- c()
    msgs <- c()

    if (init) detach.all()

    for (pkg in packages) {
      rm(list = ls(pattern = "^repo$"))
      if (regexpr("/", pkg) > 0) {
        pkg <- unlist(strsplit(pkg, split = "/", fixed = T))
        repo <- pkg[1]
        pkg <- pkg[2]
      }

      inst.result <- tryCatch(
        if (!pkg %in% installed.packages()[, "Package"]) {
          if (exists("repo")) {
            devtools::install_github(paste(repo, pkg, sep = "/"))
          } else {
            utils::install.packages(pkg)
          }
        },
        warning = function(w) {
          msgs <<- c(msgs, w$message)
          return(pkg)
        },
        error = function(e) {
          msgs <<- c(msgs, e$message)
          return(pkg)
        },
        finally = {
          if (pkg %in% installed.packages()[, "Package"]) {
            ld.result <- tryCatch({
              capture.output(suppressMessages(do.call("library", list(pkg))))
            }, error = function(e) {
              msgs <<- c(msgs, e$message)
              return(e)
            }, warning = function(w) {
              msgs <<- c(msgs, w$message)
              return(w)
            })
          }
        }
      )

      inst.errs <- c(inst.result, inst.errs)
      ld.errs <- c(ld.result, ld.errs)
    }

    if (length(inst.errs) > 0) {
      warning(
        paste(
          "There were issues installing the following package(s):",
          paste(inst.errs, collapse = ", ")
        ),
        immediate. = T
      )
    }

    if (length(ld.errs) > 0) {
      warning(paste(
        "There were issues loading the following package(s):",
        paste(ld.errs, collapse = ", ")
      ),
      immediate. = T)
    }

    if ((length(msgs) > 0) & messages) {
      msgs <- paste(msgs, collapse = "\n\n")
      msgs <- paste0("\nMessages:", gsub("^|\n", "\n\t", msgs))
      message(msgs)
    }
  }

