

mipplot_setup <- function(target.package,
                                           repos="https://cloud.r-project.org/"){

    if ( ! (target.package %in% installed.packages()[,"Package"]) ){
        #print("Package missing! Installation will proceed!")
        install.packages(target.package,repos=repos)

    }
    require(target.package, character.only = T)
}


# END
