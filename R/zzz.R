.onLoad <- function(libname, pkgname) {
  register_extra_blocks(pkgname)
  invisible(NULL)
}
