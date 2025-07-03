
#' Run_Stock_Synthesis
#' @description
#' @param Origin The working directory on a network or hard drive where the
#'     Stock Synthesis assessment is being performed. The SS files will be copied
#'     from here to `SS_dr` where the model is run. If `copy_to_origin == TRUE`
#'     (default) then the SS model files are copied back to this directory.
#' @param SS_dr The location on the hard drive where the SS model will be run locally.
#'     make sure there is no white space in this filepath, especially if using a
#'     non Windows computer.
#' @param SS_loc The location of the SS executable file.
#' @param do_hess Should the hessian be calculated
#' @param do_forecast Should forecasts be included in the `r4ss` outputs?
#' @param plots Do you want to produce the `r4ss` outputs?
#' @param ss_exe The name of the SS executable. The default is 'ss3_osx_arm64'
#'     which corresponds to my own operating system.
#' @param print_r4SS_to_screen How much information from `r4ss` would you like
#'     vommitted back at you?
#' @param print_updates_to_screen How much information from the SS model run would
#'     you like vommitted back at you.
#' @param Ask Set to `TRUE` (default) so that the function prompts you to check the
#'      `SS_dr` is correct. All existing files are erased from here at the beginning
#'      of the function call so this is way of avoiding accidentally wiping files from
#'      an incorrect file path.
#' @param copy_to_origin Do you want the SS files (and `r4ss` plots if requested)
#'     copied back to the `Origin`? This can take a while if working remotely with
#'     an agency network.
#' @param ... Function calls to be passed to `r4ss`
#'
#' @import crayon r4ss
#' @returns An SS model run saved to the relevant directories as well as `r4ss`
#'     plots if requested. No R objects are returned.
#' @export
#'
#' @examples
Run_Stock_Synthesis <- function(Origin = getwd(),
                          SS_dr = NULL,
                          SS_loc =  '/Users/jonathansmart/Documents/SS/',
                          do_hess = TRUE,
                          do_forecast = FALSE,
                          plots = TRUE,
                          ss_exe = "ss3_osx_arm64",
                          print_r4SS_to_screen = FALSE,
                          print_updates_to_screen = TRUE,
                          Ask = TRUE,
                          copy_to_origin = TRUE,
                          ...){

  Original_WD <- getwd()

  # Error message if SS directory isn't specified
  if(is.null(SS_dr)) stop("SS directory not specified")

  # Create the SS_dr if it doesn't exist
  # If it does exist, ask the user if they want to delete the files and re-run SS
  if(!dir.exists(SS_dr)){
    dir.create(SS_dr,, recursive = TRUE)
    if(dir.exists(SS_dr))
      cat(paste(crayon::green("\n\u2713"),crayon::blue(paste(SS_dr,"has been created\n"))))
  } else{
    if(Ask){
      choice <- menu(c("Yes", "No"),
                     title=paste("This function will delete all files from",SS_dr,"and then run fresh models from this directory\n do you want to proceed?"))

      if(choice !=1) return(print("Process aborted. No files have been deleted"))
    }
  }

  #If the drives are the same skip this step. Only needed for use in future functions
  if( Origin != SS_dr){
    # Remove previous model files from SS directory to avoid issues.
    # Just leave the ss3.exe file
    #Remove r4ss plots subfolder too
    Old_SS_files <- list.files(SS_dr, full.names = TRUE)#[grep(".exe",invert = T, list.files(SS_dr, full.names = TRUE))]
    unlink(Old_SS_files, recursive = TRUE)


    # Copy the SS input files from source directory
    r4ss::copy_SS_inputs(dir.old = Origin,
                   dir.new = SS_dr,
                   verbose = print_updates_to_screen)
  }
  # Run SS3 from within the SS directory
  # Calculate hessian matrix if specified

  executable_path <- paste0(SS_loc,ss_exe)

  if(do_hess){
    command <- paste0("chmod +x ", executable_path, " ; cd ", SS_dr, "; ../",ss_exe)
  } else{
    command <- paste0("chmod +x ", executable_path, " ; cd ", SS_dr, "; ../",ss_exe, " -nohess -nox")
  }

  if(print_updates_to_screen)
    cat(crayon::blue("\nRunning Stock Synthesis\n"))
  setwd(SS_dr)
  system(command, intern=!print_r4SS_to_screen)

  setwd(Origin)

  if(!file.exists(file.path(SS_dr,"Report.sso"))) stop("Stock Synthesis has not run. Check input files")
  if(print_updates_to_screen)
    cat(paste(crayon::green("\n\u2713"),crayon::blue("SS has run\n")))

  if(plots){
    Replist <- r4ss::SS_output(SS_dr,verbose = print_r4SS_to_screen, printstats = print_r4SS_to_screen, forecast = do_forecast)

    r4ss::SS_plots(Replist,verbose = print_r4SS_to_screen, forecastplot = do_forecast)

    cat(paste(crayon::green("\n\u2713"),crayon::blue("r4ss has produced all plots\n")))


  }

  setwd(Original_WD)

  if(copy_to_origin){
    cat(crayon::blue("\nSS files are being copied to network\n"))
    New_SS_files <- list.files(SS_dr, full.names = TRUE)[grep(".exe",invert = T, list.files(SS_dr, full.names = TRUE))]
    # Copy files back to the original folder
    file.copy(New_SS_files, Origin, overwrite = TRUE, recursive = T)
    # cat("\n\u2713 File transfer complete\n")
    cat(paste(crayon::green("\n\u2713"),crayon::blue("File transfer complete\n")))
  }
}
