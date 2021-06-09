#' @section Package options:
#'
#' See [options()] for information on how to modify these:
#'
#' \describe{
#'  \item{system_cmd}{
#'    See `?system_cmd`.
#'  }
#'  \item{system_nm}{
#'    See `?system_nm`.
#'  }
#'  \item{quiet_run}{
#'    Sets the default value for the `quiet` argument in `run_nm`.
#'  }
#'  \item{intern}{
#'    Sets the default value for the `intern` argument in `run_nm`.
#'  }
#'  \item{available_nm_types}{
#'    A list of NONMEM subroutine names.
#'    This is modifiable in case future versions of NONMEM contain new
#'    subroutines.
#'  }
#'  \item{nm_default_dirs}{
#'    An optional (partially) named list of directories
#'    to be used by `nm_create_analysis_project` to create the project
#'    directory structure when making a new analysis project.
#'    Required names are `"models"`, `"scripts"` and `"results"`.
#'    By default these will be set to `"Models"`, `"Scripts"` and `"Results"`,
#'    respectively.  Additional characters (e.g. `"SourceData"`) correspond to
#'    additional modelling directories to be created.
#'  }
#'  \item{kill_job}{
#'    An optional function to kill jobs before they are started.
#'    This is useful in infrastructures, where repeating a job conflicts with
#'    other jobs writing into the same directories.  For example, in Slurm grids
#'    this could be a function wrapper around a `system("scancel ...")`
#'    call.
#'  }
#'  \item{nm.overwrite_behaviour}{
#'    See `?overwrite_behaviour`.
#'  }
#'  \item{nm.force_render}{
#'    Sets default value for the `force` argument in See `nm_render`.
#'  }
#'  \item{nm.cmd_default}{
#'    Sets default `cmd` field for all nm objects.
#'    See `?cmd` for more information.
#'    The default value is "execute {ctl_name} -dir={run_dir}".
#'    However in environments where a different command is needed, this is the
#'    place to change it.
#'  }
#'  \item{nmtran_exe_path}{
#'    Sets the value of the NMTRAN.exe program.  This may be deprecated in future
#'    it is recommended to use `nm_tran_command()`.
#'    See `?nm_tran_command` for more information.
#'  }
#'  \item{code_library_path}{
#'    Path to code library.  By default this will point to the code library
#'    within the NMproject installation directory.
#'    See `?code_library` for more information.
#'  }
#' }
#'
#'
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
