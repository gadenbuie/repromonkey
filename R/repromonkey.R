#' Summon the Repro Monkey
#'
#' Summon the repro monkey to lurk in your workspace. He always asks for
#' consent first. The easiest way is to add `repromonkey` to your
#' `.Rprofile` so that you'll always be on your toes when working.
#'
#' @section Warning:
#' This is a fun tool to help bolster your confidence as you follow and hone
#' your reproducibility best practices skills. But it does come at the risk of
#' data loss and frustration (just like the real world). Your consent is
#' requested at each R session restart.
#'
#' @section Add to `.Rprofile`:
#' To summon the repro monkey automatically, edit your `.Rprofile` and add
#' the following lines. (If you use the library `usethis`, you can can call
#' `usethis::edit_r_profile()`.)
#'
#' ```
#' if (interactive() && requireNamespace("repromonkey", quietly = TRUE)) {
#'   repromonkey::repromonkey()
#' }
#' ```
#'
#' If you want to waive the interactive consent, add `consented = TRUE` to the
#' call to `repromonkey()`.
#'
#' Note that you can also choose to add **repromonkey** to specific projects
#' using project-specific `.Rprofile` files.
#'
#' @param wait Set a deterministic wait time in seconds. If `NULL`, a random
#'   wait time is selected from an exponential distribution with average wait
#'   time of 2 hours (but truncated at 3 hours). So by default, the repro
#'   monkey will strike within three hours.
#' @param delay_self A wait period of a few seconds to allow startup scripts to
#'   complete before requesting consent.
#' @param consented Have you consented? If `TRUE`, no consent will be requested
#'   prior to summoning the repro monkey.
#' @export
repromonkey <- function(wait = NULL, delay_self = 5, consented = FALSE) {
  if (delay_self && !consented) {
    later::later(~ repromonkey(wait, 0, consented), delay_self)
    return(invisible())
  }
  if (!consented && !get_consent()) {
    cat("Cool, ")
    monkey_did("wanted to take a nap anyway")
    return(invisible())
  }

  monkey_did("lurks")

  if (is.null(wait)) {
    wait <- stats::rexp(1, 1/(60^2 * 2))
    if (wait > 60^2 * 3) wait <- 3*60^2 - sample(-60:-1, 1)
  }

  later::later(monkey_around, wait)
}

#' @describeIn repromonkey Provides instructions on how to install repromonkey.
#' @export
install_repromonkey <- function() {
  cat("\nTo install repromonkey, open `", path.expand("~/.Rprofile"), "` ",
      "and add the following lines:\n\n",
      'if (interactive() && requireNamespace("repromonkey", quietly = TRUE)) {\n',
      "  repromonkey::repromonkey()",
      "\n}",
      sep = "")
}

monkey_around <- function(chaos = NULL) {
  actions <- c("restart", "clearws", "scramble", "taunt", "stash", "detach")
  chaos <- if (is.null(chaos)) sample(c("restart"), 1)
  monkey_did("was heard nearby")
  delay <- sample(10:20, 1)
  later::later(~ summon_chaos_monkey(chaos), delay)
  repromonkey(delay_self = delay, consented = TRUE)
}

summon_chaos_monkey <- function(chaos = "restart") {
  switch(
    chaos,
    restart  = monkey_restart(),
    clearws  = monkey_clear_workspace(),
    scramble = monkey_scramble_workspace(),
    stash    = monkey_stash(),
    detach   = monkey_detach(),
    taunt    = monkey_did("*may* have done some tinkering with your code"),
    monkey_did("got distracted")
  )
  .repromonkey$last <- chaos
}

get_consent <- function() {
  consent <- yesno::yesno("repro monkey asks: Can you handle a small dose of chaos?")
  if (is.null(consent)) FALSE else consent
}

monkey_did <- function(msg = "bides his time") {
  cat(paste0("repro monkey ", msg, "...\n"))
}

in_rstudio <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
}

monkey_clear_workspace <- function() {
  monkey_did("decided to clean up your workspace for you")
  base::rm(list = base::ls())
}

monkey_restart <- function() {
  monkey_clear_workspace()

  if (in_rstudio()) {
    monkey_did("accidentally unplugged RStudio")
    proj <- NULL
    if (rstudioapi::hasFun("getActiveProject")) {
      proj <- rstudioapi::getActiveProject()
    }
    if (!is.null(proj)) rstudioapi::openProject(proj) else .rs.restartR()
  } else {
    monkey_did("power cycled your R session")
    { system("R"); q("no") }
  }
}

monkey_scramble_workspace <- function() {
  if (!length(base::ls(envir = .GlobalEnv))) return(monkey_did())

  shadow_env <- new.env()

  # copy global vars to shadow env
  vars <- base::ls(envir = .GlobalEnv)
  var_type <- base::vapply(vars, function(x) {
    cc <- base::class(base::get(x, envir = .GlobalEnv))
    cc[length(cc)]
  }, character(1))
  vars <- vars[var_type != "function"]
  for (var in base::ls(envir = .GlobalEnv)) {
    base::assign(var, base::get(var, envir = .GlobalEnv), envir = shadow_env)
  }

  # shuffle global vars
  vars <- stats::setNames(vars, sample(vars, length(vars)))
  for (i in seq_along(vars)) {
    base::assign(
      names(vars[i]),
      base::get(vars[i], envir = shadow_env),
      envir = .GlobalEnv
    )
  }

  monkey_did("played 52-card pickup with your global environment")
}

monkey_stash <- function() {
  if (!in_rstudio()) monkey_did("tried to swipe your source code but missed")

  open_doc <- rstudioapi::getSourceEditorContext()
  if (open_doc$path != "") {
    monkey_did("saw you were hard at work and decided to leave you alone")
  }

  .rs.rpc.save_active_document(open_doc$contents, FALSE)
  dirs <- get_directories()
  f_dir <- sample(dirs$path, 1)
  f_name <- sample(monkey_names, 1)
  f_path <- file.path(f_dir, paste0(f_name, ".R"))
  file.copy(path.expand("~/.active-rstudio-document"), f_path)
  .rs.api.documentClose(open_doc$id, FALSE)
  monkey_did("stashed your unsaved code somewhere safe")
  .repromonkey$hint <- f_path
}

get_directories <- function(base_dir = getwd(), recursive = TRUE) {
  files <- dir(path = base_dir, include.dirs = TRUE, full.names = TRUE, recursive = recursive)
  files <- c(base_dir, files)
  file_info <- lapply(files, function(x) {
    fi <- file.info(x)
    data.frame(path = x, isdir = fi$isdir, stringsAsFactors = FALSE)
  })
  file_info <- do.call("rbind", file_info)
  dirs <- file_info[file_info$isdir, ]
  dirs
}

monkey_detach <- function() {
  if (!in_rstudio()) {
    rs <- detach_packages()
  } else {
    open_doc <- rstudioapi::getSourceEditorContext()
    rs <- detach_packages(pkg_ok = called_in_code(open_doc$contents))
  }

  if (rs == "") {
    monkey_did("tried to swipe a package from you")
  } else {
    monkey_did("stole a package from you")
  }
  .repromonkey$hint <- rs
}

attached_packages <- function() {
  pkgs <- base::search()
  pkgs <- pkgs[!grepl("stats|graphics|grDevices|utils|datasets|methods|base|repromonkey", pkgs)]
  pkgs[grepl("package", pkgs)]
}

called_in_code <- function(code) {
  # borrowed from {automagic}: https://github.com/cole-brokamp/automagic/blob/master/R/parse_packages.R
  code_lib <- regmatches(code, gregexpr('(?<=(library\\()|(library\\(["\']{1}))[[:alnum:]|.]+', code, perl=TRUE))

  code_req <- regmatches(code, gregexpr('(?<=(require\\()|(require\\(["\']{1}))[[:alnum:]|.]+', code, perl=TRUE))

  code_cln <- regmatches(code, gregexpr("[[:alnum:]|.]*(?=:{2,3})", code, perl=TRUE))

  pkgs <- unique(unlist(list(code_lib, code_req, code_cln)))
  pkgs[pkgs != ""]
}

detach_packages <- function(pkg_detach = attached_packages(), pkg_ok = NULL) {
  pkg_ok[!grepl("^package:", pkg_ok)] <- paste0("package:", pkg_ok[!grepl("^package:", pkg_ok)])
  pkg_detach <- pkg_detach[!pkg_detach %in% pkg_ok]
  if (!length(pkg_detach)) return("")
  detached_a_package <- FALSE
  while (length(pkg_detach) > 0 && !detached_a_packge) {
    pkg <- sample(pkg_detach, 1)
    detached_a_package <- tryCatch({
      detach(pkg)
      TRUE
    },
    warning = function(w) TRUE,
    error = function(e) FALSE)
    if (!detached_a_package) {
      pkg_detach <- setdiff(pkg_detach, pkg)
    }
  }
  if (detached_a_package) pkg else ""
}
