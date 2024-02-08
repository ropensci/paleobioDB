.onLoad <- function(libname, pkgname) {
  # This sets up the .private_pk_cache environment, but since all
  # endpoints use the same uri_builder function anyway, there is not
  # that much use to it other than having a structure that informs
  # about the mandatory parameters (compulsory_params).
  #
  # However, it also seems that the internal function that checks the
  # compulsory parameters is not doing anything right now.
  .pbdb_setup()
}
