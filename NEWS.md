0.2.5.001
===================

## Minor changes

- `ctags` and `gtags` output is read with `quote = ""`, so tags whose content contains quotes are kept; a single line of `gtags` output is read as data, not as a file path (#127, @rootcoder007).
- `ctags_test()` splits fields on tabs and expects the `vector` kind (#127, @rootcoder007).
- Local variables are removed from function calls by source line in every file; calls to recommended packages are matched against the whole table, and imported functions are matched exactly (#127, @rootcoder007).
- The call-network summary counts each edge once: edge, node-degree, terminal-edge and centrality statistics all come from the de-duplicated edges, so `n_edges_r + n_edges_src == n_edges` (#127, @rootcoder007).

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
