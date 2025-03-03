if (interactive() && Sys.getenv("RSTUDIO") == "") {
  source(file.path(Sys.getenv(
    if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"
  ), ".vscode-R", "init.R"))
  if ("httpgd" %in% .packages(all.available = TRUE)) {
    options(vsc.plot = FALSE, vsc.use_httpgd = FALSE)
    options(device = function(...) {
      httpgd::hgd(silent = TRUE)
      .vsc.browser(httpgd::hgd_url(history = FALSE), viewer = FALSE)
    })
  }
}

source(file.path(Sys.getenv(
   if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"
 ), ".vscode-R", "init.R"))