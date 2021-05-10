## Resubmission

This is a resubmission. In this version I have:

* Converted T and F to TRUE and FALSE.

* Added \value to .Rd files.

* Omitted examples for unexported functions in
  * get_model_name_list()
  * get_scenario_name_list()
  * get_string_expression_of_vector_of_strings()

* Removed unnecessary \dontrun{}.

* Converted some \dotnrun{} to \donttest{}.

* Set eval=FALSE in the chunk in vignette which accesses user's folder.

* Move some contributor names in mipplot_return_table_github.R to README.md.
  * SNEH DESHPANDE and KEITARO HANZAWA gave their code for us so we didn't add them to Authors@R field.

## Test environments

* local: windows-x86_64-release
* r-hub: windows-x86_64-devel, ubuntu-gcc-devel, debian-gcc-devel, fedora-gcc-devel
* win-builder: x86_64-w64-mingw32

## R CMD check results

0 errors | 0 warnings | 1 notes

## revdepcheck results

There are currently no downstream dependencies for this package
