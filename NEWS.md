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
