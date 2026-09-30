if (!requireNamespace("rsconnect", quietly = TRUE)) {
  stop("Le package R 'rsconnect' est requis pour générer manifest.json.")
}

script_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (!length(script_argument)) {
  stop("Exécuter ce script avec Rscript.")
}
script_path <- sub("^--file=", "", script_argument[1])
project_directory <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/")

rsconnect::writeManifest(
  appDir = project_directory,
  appFileManifest = file.path(project_directory, "deploy-files.txt"),
  appPrimaryDoc = "app.R", appMode = "shiny", dependencyResolution = "library"
)
