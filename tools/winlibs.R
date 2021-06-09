if(!file.exists("../windows/universal-ctags-5.9.20210530.0/bin")){
  download.file("https://github.com/rwinlib/universal-ctags/archive/refs/tags/v5.9.20210530.0.zip",
                "lib.zip", quiet = TRUE)
  dir.create("../windows", showWarnings = FALSE)
  unzip("lib.zip", exdir = "../windows")
  unlink("lib.zip")
}
