source(".Rprofile")

file.copy("C:/code/external/kinetic-t2d/data/ktdat06_cosmos county estimates.csv",
          paste0(path_kiosk_user_patterns_repo,"/data/kupdat08_cosmos county estimates.csv"))


file.copy("C:/code/external/kinetic-t2d/data/ktdat06_cosmos overall estimates.csv",
          paste0(path_kiosk_user_patterns_repo,"/data/kupdat08_cosmos overall estimates.csv"))
