## Copied from staging/Scripts/starter_ewf.R
##  (2021-03-02 13:53:56) by tarjinde
## Copied from staging/Scripts/starter_ewf.R
##  (2020-04-22 19:23:42) by klgk669
#  set up .libPaths() to find the PoC version of the ewfutils library
.libPaths( c( "/opt/scp/services/ewfscp/software/ewfutils-2.0-poc/libraries/R", .libPaths()) )

# load ewfutils
library(ewfutils)

#  load ewf modules
ewf_scp_module_load( "ewfscp" )
ewf_scp_module_load( "ewf/2.0-poc" )

# load rseed library
library(rseed)

# connect to the example workspace
system.time({
  wksp <- ewf_scp_workspace(basename(getwd()))
})

# connect to the development instance of EntimICE
conn <- ewf_scp_entimice_connect( instance = "development" )


# download data sets
dwnld <- ewf_scp_entimice_download( connection = conn,
                                    path = "root/qcp_ewf/onc/azd1234/ar/poppk1_programming/sdtm/dev/data/data_area",
                                    files = NULL,
                                    outputformats = c("r", "csv"),
                                    workspace = wksp$subfolder("SourceData"),
                                    keepsource = TRUE,
                                    verbose = TRUE,
                                    verify = TRUE)


NMproject::setup_nm_demo()

# create a local archive
# stored in the EWF Workspace under .ewf/.archives
archive <- ewf_scp_archive( wksp, verbose = TRUE )


# create an EWF analysis area on EntimICE
system.time({
  myanalysis <- ewf_scp_entimice_analysis( connection = conn, name = basename(getwd()), path = "root/qcp_ewf/onc/azd1234")
})

# create an archive and store it in EntimICE
system.time({
  entimice_archive <- ewf_scp_entimice_archive( connection = conn, workspace = wksp, analysis = myanalysis, verbose = TRUE )
})



# create a share folder for outputs in the workspace
wksp_outputs <- wksp$subfolder("outputs", create = TRUE)


# create a dummy PDF file to share in the outputs folder
system( paste( "touch", file.path( wksp_outputs, "test.pdf"), sep = " "), intern = TRUE )


# share
shared <- ewf_scp_entimice_share( connection = conn, workspace = wksp, analysis = myanalysis, verbose = TRUE )


# disconnect from EntimICE
conn$disconnect()
