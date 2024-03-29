% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tag-data.R
\name{tags_data}
\alias{tags_data}
\title{use ctags and gtags to parse call data}
\usage{
tags_data(path, has_tabs = NULL, pkg_name = NULL)
}
\arguments{
\item{path}{Path to local repository}

\item{has_tabs}{A logical flag indicating whether or not the code contains
any tab characters. This can be determined from \link{loc_stats}, which has a
\code{tabs} column. If not given, that value will be extracted from internally
calling that function.}

\item{pkg_name}{Only used for external_call_network, to label
package-internal calls.}
}
\value{
A list of three items:
\itemize{
\item "network" A \code{data.frame} of relationships between objects, generally as
calls between functions in R, but other kinds of relationships in other
source languages. This is effectively an edge-based network representation,
and the data frame also include network metrics for each edge, calculated
through representing the network in both directed (suffix "_dir") and
undirected (suffix "_undir") forms.
\item "objects" A \code{data.frame} of statistics on each object (generally
functions in R, and other kinds of objects in other source languages),
including the kind of object, the language, numbers of lines-of-code,
parameters, and lines of documentation, and a binary flag indicating whether
or not R functions accept "three-dots" parameters (\code{...}).
\item "external_calls" A \code{data.frame} of every call from within every R
function to any external R package, including base and recommended packages.
The location of each calls is recorded, along with the external function and
package being called.
}
}
\description{
use ctags and gtags to parse call data
}
\examples{
f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
# have to extract tarball to call function on source code:
path <- extract_tarball (f)
\dontrun{
tags <- tags_data (path)
}
}
\seealso{
Other tags: 
\code{\link{ctags_install}()},
\code{\link{ctags_test}()}
}
\concept{tags}
