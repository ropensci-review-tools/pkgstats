% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loc.R
\name{loc_stats}
\alias{loc_stats}
\title{Internal calculation of Lines-of-Code Statistics}
\usage{
loc_stats(path)
}
\arguments{
\item{path}{Directory to source code of package being analysed}
}
\value{
A list of statistics for each of three directories, 'R', 'src', and
'inst/include', each one having 5 statistics of total numbers of lines,
numbers of empty lines, total numbers of white spaces, total numbers of
characters, and indentation used in files in that directory.
}
\description{
Internal calculation of Lines-of-Code Statistics
}
\note{
NA values are returned for directories which do not exist.
}
\examples{
f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
# have to extract tarball to call function on source code:
path <- extract_tarball (f)
loc_stats (path)
}
\seealso{
Other stats: 
\code{\link{desc_stats}()},
\code{\link{pkgstats}()},
\code{\link{pkgstats_summary}()},
\code{\link{rd_stats}()}
}
\concept{stats}
