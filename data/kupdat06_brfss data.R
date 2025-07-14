rm(list=ls());gc();source(".Rprofile")



path_diabetes_eye_screening_folder = "C:/Users/jvargh7/Box/Papers/Diabetes Eye Screening Demand and Supply"

brfss2023 = haven::read_xpt(paste0(path_diabetes_eye_screening_folder,"/working/raw/LLCP2023.xpt"))
