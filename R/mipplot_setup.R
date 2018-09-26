# TITLE: JP MIP submission tool setup.
# OUTLINE: funtion to prepare the JP MIP submission tool.
# Developed by: Masahiro SUGIYAMA and Diego SILVA HERRAN.
# Last revision by: XX.

mipplot_setup <- function(target.package,
                                           repos="https://cloud.r-project.org/"){

    if ( ! (target.package %in% installed.packages()[,"Package"]) ){
        #print("Package missing! Installation will proceed!")
        install.packages(target.package,repos=repos)

    }
    require(target.package, character.only = T)
}


# END
