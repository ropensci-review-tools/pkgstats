% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cran-data-fn-names.R
\name{pkgstats_fns_from_archive}
\alias{pkgstats_fns_from_archive}
\title{Trawl a local CRAN archive to extract function names only from all packages}
\usage{
pkgstats_fns_from_archive(
  path,
  archive = FALSE,
  prev_results = NULL,
  results_file = NULL,
  chunk_size = 1000L,
  num_cores = 1L,
  results_path = fs::path_temp()
)
}
\arguments{
\item{path}{Path to local archive of R packages, either as source
directories, or '.tar.gz' files such as in a CRAN mirror.}

\item{archive}{If \code{TRUE}, extract statistics for all packages in the
\verb{/Archive} sub-directory, otherwise only statistics for main directory (that
is, current packages only).}

\item{prev_results}{Result of previous call to this function, if available.
Submitting previous results will ensure that only newer packages not present
in previous result will be analysed, with new results simply appended to
previous results. This parameter can also specify a file to be read with
\code{readRDS()}.}

\item{results_file}{Can be used to specify the name or full path of a \code{.Rds}
file to which results should be saved once they have been generated. The
'.Rds' extension will be automatically appended, and any other extensions
will be ignored.}

\item{chunk_size}{Divide large archive trawl into chunks of this size, and
save intermediate results to local files. These intermediate files can be
combined to generate a single \code{prev_results} file, to enable jobs to be
stopped and re-started without having to recalculate all results. These files
will be named \code{pkgstats-results-N.Rds}, where "N" incrementally numbers each
file.}

\item{num_cores}{Number of machine cores to use in parallel, defaulting to
single-core processing.}

\item{results_path}{Path to save intermediate files generated by the
\code{chunk_size} parameter described above.}
}
\value{
A \code{data.frame} object with one row for each function in each package
and the following columns:
\itemize{
\item Package name
\item Package version
\item Function name
}
}
\description{
Trawl a local CRAN archive to extract function names only from all packages
}
\seealso{
Other archive: 
\code{\link{dl_pkgstats_data}()},
\code{\link{pkgstats_cran_current_from_full}()},
\code{\link{pkgstats_fns_update}()},
\code{\link{pkgstats_from_archive}()},
\code{\link{pkgstats_update}()}
}
\concept{archive}
