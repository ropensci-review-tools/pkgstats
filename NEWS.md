0.2.5.00x (dev release)
===================

## Bug fixes (@rootcoder007)

- ctags and gtags output is read with no quote character, so tags following a `"` in source code are no longer dropped or merged. `ctags_test()` now also expects the "vector" tag which that bug dropped.
- Local variables are removed from external calls of all functions, by matching source line numbers instead of row numbers of the tags table.
- Calls to recommended packages (such as 'MASS' and 'rpart') are matched against the full table of recommended functions.
- Imported function names are matched exactly rather than as regular expressions, so `importFrom(pkg, "[.data.table")` no longer errors.
- `pkgstats_summary()` forces column types of all summaries, not only of the null summary, so dates are no longer lost when trawl results are combined.
- `pkgstats_summary()` no longer uses `grepv()`, which requires R >= 4.5.
- `n_edges` and `node_degree_*` count unique edges, as documented, rather than edge multiplicity.
- Documentation lines from ctags are assigned to the correct R functions.
- Packages with no "man" directory can be analysed.
- `rm_tabs()` writes the de-tabbed text.
- Function names and versions are read from a package's own "NAMESPACE" and "DESCRIPTION" files, not those of test fixtures.
- `get_pkg_version()` returns `NA` for paths which are not tarballs, so failed analyses in `pkgstats_update()` are named, and no longer recorded with an empty package name.
- Archive trawls drop (with a warning) results of parallel jobs which fail, instead of aborting.
- `pkgstats_update()` forces column types of new results before appending them to previous results, so the `max_loss` check applies to new results.
- `pkgstats_fns_update()` downloads new tarballs before extracting function names.
- `pkgstats_fns_from_archive()` only lists tarballs, and no longer errors when there are no new tarballs.
- Results file names may contain regular expression characters.
- `pkgstats_from_archive()` accepts a `data.frame` as `prev_results`, as documented.
- gtags results from vendored source paths are included in the network (#104).

0.2.5
===================

## Minor changes

- Added `inst/extdata/pkgstats-col-types.csv` dictionary to assert and enforce column types in summary data.

0.2.4
===================

## Major changes

- `plot_network()` no longer depends on the 'visNetwork' package; the interactive network visualisation is now rendered with a bundled 'D3'-based JavaScript skeleton shipped in `inst/js` (see #118)
- That new network plot includes additional interactive capabilities, like function selection and highlighting.

## Minor changes

- New internal `force_col_types()` function to assert and force-convert col types to a fixed dictionary (#121; thanks to @rootcoder007)
- Improve diagnosis of wrong ctags version on MacOS (#115; thanks to @mcol)

0.2.3
===================

## Minor changes

- Fix bug in source docline count when there are no doclines
- Fix output of translations in summary fn
- Fix bug in `dl_pkgstats_data()` - defaulted to latest release, but data are fixed for a previous release.
- Remove 'ami' as dependency (#112), as it got archived on CRAN; 'on_cran()' fn now internal here.

0.2.2
===================

## Minor changes

- Improved documentation line counting for R source files (#81, #97)
- Detect and parse Quarto vignettes (#95)
- Better handling of temporary directories in archive extraction (#93)

0.2.1
===================

## Major changes

- Former 'configure' and 'Makevars.win' files which used to download ctags binaries on package install now removed, and replaced with internal functions in `zzz.R` called on first package load.

## Minor changes

- Added this news file
- Parse 'Authors@R' field of DESC even when `parse()` fails
- Fix bug on CRAN packages with no DESCRIPTION file (#51; these do exist!)
- Update all file path manipulations to use 'fs' pkg (#68)
- Improve lines-of-code counts, including capturing tinytest files, and remove others (#67)


0.1.1
===================

## Minor changes

- Hard-code main vignette because it can't be built on CRAN windows machines
- Minor bug fix with sometimes failing tag extraction from `inst` dir

0.0.5
===================

## Initial CRAN Release
