usethis::use_release_issue()
usethis::use_cran_comments()
devtools::check(remote = T)
devtools::devtools::check_win_devel()

rhub::check_for_cran()
