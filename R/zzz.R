.onLoad <- function(libname, pkgname) {

  if(Sys.getenv("rb") == "")
     cat("Thank you for using the Revticulate package!\n\nTo interact with RevBayes, you must first add the RevBayes path to R PATH variable.\n\nTo do this, run the function `usethis::edit_r_environ()` in your console. A file called `.Renviron` should open up.\n\nIn this file, type rb={file path to the RevBayes executable}. Do not put quotations around the filepath.\n\nSave the file, and restart the R session.")

  if(!file.exists(Sys.getenv("rb"))){
    cat("A path to RevBayes has been provided, but does not seem to exist at the specified location.\n\nPlease use `usethis::edit_r_environ()`, and change the RevBayes path in the `.Renviron` file.\n\nThen restart the R session after providing the correct path.")
  }

  Sys.setenv(revDir = tempdir())

  Sys.setenv(revHistory = (tempdir() %+% "/.Revhistory"))

  Sys.setenv(revTemps = (tempdir() %+% "/temps"))

  Sys.setenv(revSeed = sample(1:100000, 1))

  if(!dir.exists(Sys.getenv("revDir"))){
    dir.create(Sys.getenv("revDir"))
  }

  if(!dir.exists(Sys.getenv("revTemps"))){
    dir.create(Sys.getenv("revTemps"))
  }

  if(!file.exists(Sys.getenv("revHistory"))){
    file.create(Sys.getenv("revHistory"))
    cat("#START\n", file = Sys.getenv("revHistory"))
  }

}
