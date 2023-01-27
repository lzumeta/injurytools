## Test environments

* local ubuntu 18.04.6 install, R 4.2.2
* Appveyor (patched)
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 note

## CRAN submission 1.0

Documentation fixes and a minor fix (related to a 'invalid file URI') were requested. Bullets below detail changes that were made to address these requests.

* Fixed the issue with 'invalid file URI'.
* Replaced \dontrun{} with \donttest{} for slow examples. The main user-facing functions all have examples that are tested, the \donttest{} examples just provide additional documentation for user on how to use the software for a large real-world data set.
* Have added \value section to all .Rd files that are not data files to explain the function results.

## Reverse dependencies

There are no reverse dependencies
