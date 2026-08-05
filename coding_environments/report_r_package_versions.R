# report_r_package_versions.R
#
# Scans all Rmd files in analysis_and_plots/ for library() calls, then writes
# a txt summary of R version, platform, and every package version.
#
# Run from the repo root:
#   Rscript coding_environments/report_r_package_versions.R
#
# Output: coding_environments/r_package_versions_<YYYY-MM-DD>.txt

# ── resolve repo root ──────────────────────────────────────────────────────────
# Works whether invoked from the repo root or from inside coding_environments/.
script_dir  <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)),
                        error = function(e) getwd())
repo_root   <- if (basename(script_dir) == 'coding_environments') {
  dirname(script_dir)
} else {
  script_dir
}

rmd_dir     <- file.path(repo_root, 'analysis_and_plots')
output_dir  <- file.path(repo_root, 'coding_environments')

# ── extract library() calls from all Rmd files ────────────────────────────────
rmd_files <- list.files(rmd_dir, pattern = '\\.Rmd$', full.names = TRUE)

pkgs <- character(0)
for (f in rmd_files) {
  lines   <- readLines(f, warn = FALSE)
  matches <- regmatches(lines, regexpr('library\\(([A-Za-z0-9._]+)\\)', lines))
  names   <- sub('library\\(([A-Za-z0-9._]+)\\)', '\\1', matches)
  pkgs    <- c(pkgs, names)
}
pkgs <- sort(unique(pkgs))

# ── collect version info for each package ─────────────────────────────────────
pkg_info <- lapply(pkgs, function(pkg) {
  installed <- requireNamespace(pkg, quietly = TRUE)
  if (installed) {
    ver  <- as.character(packageVersion(pkg))
    desc <- tryCatch(packageDescription(pkg), error = function(e) NULL)
    repo <- if (!is.null(desc) && !is.null(desc$Repository))  desc$Repository  else 'unknown'
    built <- if (!is.null(desc) && !is.null(desc$Built))       desc$Built       else 'unknown'
  } else {
    ver   <- 'NOT INSTALLED'
    repo  <- NA
    built <- NA
  }
  list(package = pkg, version = ver, repository = repo, built = built,
       installed = installed)
})

# ── build output lines ─────────────────────────────────────────────────────────
r_ver    <- R.version
r_string <- paste0('R version ', r_ver$major, '.', r_ver$minor,
                   ' (', r_ver$`svn rev`, ')')

lines_out <- c(
  '══════════════════════════════════════════════════════════════════════════════',
  'R ENVIRONMENT REPORT',
  paste0('Generated: ', format(Sys.time(), '%Y-%m-%d %H:%M:%S')),
  '══════════════════════════════════════════════════════════════════════════════',
  '',
  '── R version ─────────────────────────────────────────────────────────────────',
  paste0('R version:  ', R.version$version.string),
  paste0('Platform:   ', R.version$platform),
  paste0('OS:         ', Sys.info()['sysname'], ' ', Sys.info()['release']),
  paste0('Running on: ', Sys.info()['nodename']),
  ''
)

# RStudio version (only available in interactive session inside RStudio)
if (requireNamespace('rstudioapi', quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  lines_out <- c(lines_out,
    paste0('RStudio:    ', rstudioapi::versionInfo()$version),
    '')
}

lines_out <- c(lines_out,
  '── Packages ──────────────────────────────────────────────────────────────────',
  sprintf('%-30s %-15s %-12s %s', 'Package', 'Version', 'Repository', 'Built'),
  sprintf('%-30s %-15s %-12s %s', '-------', '-------', '----------', '-----')
)

for (info in pkg_info) {
  repo  <- if (is.na(info$repository)) '' else info$repository
  built <- if (is.na(info$built))      '' else info$built
  lines_out <- c(lines_out,
    sprintf('%-30s %-15s %-12s %s', info$package, info$version, repo, built))
}

# Summary counts
n_installed   <- sum(sapply(pkg_info, `[[`, 'installed'))
n_missing     <- length(pkgs) - n_installed

lines_out <- c(lines_out,
  '',
  '── Summary ───────────────────────────────────────────────────────────────────',
  paste0('Rmd files scanned:  ', length(rmd_files)),
  paste0('Unique packages:    ', length(pkgs)),
  paste0('Installed:          ', n_installed),
  paste0('Not installed:      ', n_missing)
)

if (n_missing > 0) {
  missing_pkgs <- sapply(pkg_info[!sapply(pkg_info, `[[`, 'installed')],
                         `[[`, 'package')
  lines_out <- c(lines_out,
    '',
    '── Missing packages ──────────────────────────────────────────────────────────',
    paste0('  ', missing_pkgs))
}

lines_out <- c(lines_out,
  '',
  '── Python environment ────────────────────────────────────────────────────────',
  'Python packages documented separately in: coding_environments/environment_base.yml',
  '══════════════════════════════════════════════════════════════════════════════'
)

# ── write output ───────────────────────────────────────────────────────────────
out_file <- file.path(output_dir,
                      paste0('r_package_versions_', Sys.Date(), '.txt'))
writeLines(lines_out, out_file)
cat('Written to:', out_file, '\n')
cat('R version: ', R.version$version.string, '\n')
cat('Packages scanned:', length(pkgs), '| installed:', n_installed,
    '| missing:', n_missing, '\n')
