#' Repro Monkey
#'
#' Summon the repro monkey to lurk in your workspace. It always asks for
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
#' @name repromonkey
NULL

#' @describeIn repromonkey Summon the repromonkey to lurk in your workspace.
#'   This function calls the repromonkey, by default scheduling chaos at a
#'   random time between 0 and 3 hours in the future. This function also
#'   requests explicitly consent unless otherwise provided during the current
#'   session. Set `delay_self = 0` and `consented = TRUE` to immediately invite
#'   the repromonkey into your workspace.
#' @param wait Set a deterministic wait time in seconds. If `NULL`, a random
#'   wait time is selected from an exponential distribution with average wait
#'   time of 2 hours (but truncated at 3 hours). So by default, the repro monkey
#'   will strike within three hours.
#' @param delay_self A wait period of a few seconds to allow startup scripts to
#'   complete before requesting consent.
#' @param consented Have you consented? If `TRUE`, no consent will be requested
#'   prior to summoning the repromonkey. Defaults to `FALSE` or the last consent
#'   response in the current session.
#' @param idle_timeout How often should repromonkey poll your R session history
#'   to make sure that you're actively working? The default is 15 minutes. If no
#'   activity is seen between each session poll, the repromonkey will get bored
#'   and will ignore you. This means you won't come back to your work to find
#'   that repromonkey messed it up. It doesn't _turn off_ repromonkey, so when
#'   you're back at work the monkey may come back.
#' @export
repromonkey <- function(
  wait = NULL,
  delay_self = 5,
  consented = NULL,
  idle_timeout = 900
) {
  # Bail if there's alaready a monkey around
  if (!is.null(.repromonkey$lurks)) return(invisible())

  if (in_r_package()) return(invisible())

  if (is.null(consented)) {
    consented <- isTRUE(getOption("REPROMONKEY_CONSENTED", FALSE))
  }

  if (delay_self) {
    later::later(~ repromonkey(wait, 0, consented), delay_self)
    return(invisible())
  }

  if (!consented && !get_consent()) {
    message("Cool, repro monkey wanted to take a nap anyway...")
    return(invisible())
  }

  # Show startup message
  if (!isTRUE(.repromonkey$saw_startup)) {
    monkey_did("Happy Coding! (I'll be watching you!)", monkey = TRUE)
    .repromonkey$saw_startup <- TRUE
  } else {
    monkey_did("went back to lurking")
  }

  if (is.null(.repromonkey$history_hash)) {
    idle_update(idle_timeout)
  }

  if (is.null(wait)) {
    wait <- stats::rexp(1, 1/(60^2 * 1.25))
    if (wait > 60^2 * 3) wait <- 3*60^2 - sample(-60:-1, 1)
  }

  .repromonkey$lurks <- Sys.time() + wait
  later::later(function() monkey_around(wait = wait), wait)
}

#' @describeIn repromonkey Provides instructions on how to automatically
#'   summon the repromonkey at session start.
#' @export
install_repromonkey <- function() {
  message("\nTo install repromonkey, open `", path.expand("~/.Rprofile"), "` ",
      "and add the following lines:\n\n",
      'if (interactive() && requireNamespace("repromonkey", quietly = TRUE)) {\n',
      "  repromonkey::repromonkey()",
      "\n}",
      sep = "")
}

#' @describeIn repromonkey Get a hint about when the next repromonkey will
#'   arrive or what chaos the repromonkey brought to you the last time it
#'   visited.
#' @param reveal If `FALSE`, repromonkey gives a vague hint about the next
#'   arrival or last action. If `TRUE`, repromonkey gives accurate details
#'   about what just or will happen.
#' @export
monkey_hint <- function(reveal = FALSE) {
  if (isTRUE(.repromonkey$imminent)) {
    message("REPRO MONKEY IS ABOUT TO STRIKE!!!")
    return(invisible())
  }
  ret <- NULL
  seen_once <- !is.null(.repromonkey$last)
  if (seen_once) {
    if (!reveal) {
      last_action <- switch(
        .repromonkey$last,
        restart  = "tried to restart your system",
        clearws  = "cleared your workspace",
        scramble = "messed up your workspace a bit",
        stash    = "stashed unsaved code",
        detach   = "detached a package",
        taunt    = "*may* have done some tinkering with your code",
        "got distracted"
      )
    } else {
      last_action <- switch(
        .repromonkey$last,
        restart  = "tried to restart your system",
        clearws  = "cleared your workspace with a good old-fashioned rm(list = ls())",
        scramble = "swapped the names of your workspace variables",
        stash    = if (.repromonkey$hint == "") {
          "wanted to stash your unsaved code, but you were working in a named file"
        } else {
          paste("stashed Untitled.R at", .repromonkey$hint)
        },
        detach   = if (.repromonkey$hint == "") {
          "tried to detach a package but they were all mentioned in your code"
        } else {
          paste("detached", paste(.repromonkey$hint, collapse = ", "))
        },
        taunt    = "taunted you by pretending to have tinkered with your code",
        "got distracted"
      )
    }
    if (.repromonkey$last == "scramble" && reveal) {
      ret <- .repromonkey$hint
    }
    monkey_did(last_action)
  }
  if (is.null(.repromonkey$lurks)) {
    monkey_did("is not around")
    return(invisible())
  } else {
    monkey_did(strftime(
      .repromonkey$lurks + if (reveal) 0 else floor(runif(1, -450, 450)),
      paste0("is expected ",
             if (seen_once) "again ",
             if (reveal) "at" else "around",
             " %H:%M")
    ))
  }
  if (!is.null(ret)) ret else invisible()
}

# repromonkey Package environment
.repromonkey <- new.env(parent = emptyenv())

monkey_around <- function(chaos = NULL, wait = NULL) {
  delay <- sample(10:20, 1)
  if (!is_session_idle()) {
    actions <- c("restart", "clearws", "scramble", "taunt", "stash", "detach")
    chaos <- if (is.null(chaos)) sample(actions, 1)
    monkey_did("Hey there, how's it going?", monkey = TRUE)
    later::later(~ summon_chaos_monkey(chaos), delay)
    # Repromonkey isn't lurking anymore... it's about to strike!
    .repromonkey$lurks <- NULL
    .repromonkey$imminent <- TRUE
  }
  # Schedule next repromonkey
  repromonkey(wait = wait, delay_self = delay + 1, consented = TRUE)
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
  .repromonkey$imminent <- FALSE
}

get_consent <- function() {
  message(monkey_say("Can you handle a small dose of chaos?"))
  consent <- yesno::yesno("Enable repromonkey?")
  if (is.null(consent)) FALSE else consent
}

monkey_did <- function(msg = "bides his time", monkey = FALSE) {
  if (monkey) {
    message(monkey_say(msg))
  } else {
    message(paste0("repro monkey ", msg, "..."))
  }
}

monkey_say <- function(msg = "") {
  # thanks: https://www.asciiart.eu/animals/monkeys
  sprintf(
    '
 %s
               \\
                \\
                 \\
                  \\     .="=.
                      _/.-.-.\\_     _
                     ( ( o o ) )    ))
                      |/  "  \\|    //
      .-------.        \\\'---\'/    //
     _|~~jgs~~|_       /`"""`\\   ((
   =(_|_______|_)=    / /_,_\\ \\   \\\\
     |:::::::::|      \\_\\\\_\'__/ \\  ))
     |:::::::[]|       /`  /`~\\  |//
     |o=======.|      /   /    \\  /
     `"""""""""`  ,--`,--\'\\/\\    /
                   \'-- "--\'  \'--\'
', paste(strwrap(msg), collapse = "\n ")
  )
}

in_rstudio <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
}

in_r_package <- function(dir = getwd()) {
  desc_file <- file.path(dir, "DESCRIPTION")
  file.exists(desc_file) && any(grepl("^Package:", readLines(desc_file)))
}

monkey_clear_workspace <- function() {
  monkey_did("decided to clean up your workspace for you")
  base::rm(list = base::ls(envir = .GlobalEnv), envir = .GlobalEnv)
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

  .repromonkey$hint <- data.frame(new = unname(vars), old = names(vars),
                                  stringsAsFactors = FALSE)
  monkey_did("played 52-card pickup with your global environment")
}

monkey_stash <- function() {
  if (!in_rstudio()) {
    monkey_did("tried to swipe your source code but missed")
    return(invisible())
  }

  open_doc <- rstudioapi::getSourceEditorContext()
  if (open_doc$path != "") {
    monkey_did("saw you were hard at work and decided to leave you alone")
    .repromonkey$hint <- ""
    return(invisible())
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
  while (length(pkg_detach) > 0 && !detached_a_package) {
    pkg <- sample(pkg_detach, 1)
    detached_a_package <- tryCatch({
      detach(pkg, unload = TRUE, character.only = TRUE)
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
