% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract-tarball.R
\name{extract_tarball}
\alias{extract_tarball}
\title{Extract tarball of a package into temp directory and return path to extracted
package}
\usage{
extract_tarball(tarball)
}
\arguments{
\item{tarball}{Full path to local tarball of an R package.}
}
\value{
Path to extracted version of package (in \code{tempdir()}).
}
\description{
Extract tarball of a package into temp directory and return path to extracted
package
}
\examples{
f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
path <- extract_tarball (f)
}
\seealso{
Other misc: 
\code{\link{pkgstats_fn_names}()}
}
\concept{misc}
