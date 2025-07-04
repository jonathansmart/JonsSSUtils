
#' Run_Stock_Synthesis
#' @description The Stock Synthesis software is getting harder to run on modern
#'     IT setups given fears around malicious software. Often stock assessments are
#'     best developed in a shared space such as a network or repository. However,
#'     often the SS executable can't be run from these locations and a specific
#'     directory on a computer is needed so that the file can be marked as safe.
#'     Having worked in many agencies, each with different IT permissions, the best
#'     way I've found to handle this is to develop the SS files in whatever location
#'     is most convenient (such as a shared network directory), copy those files to
#'     the safe folder where SS can be run, and then copy the SS files back to the
#'     original location. This is a bit circular but it works, and this code makes that
#'     process simple. It will also produce the `r4ss` plots in one hit and transfer
#'     these too.
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

  # Check SS_loc ends with a '/'
  if (!endsWith(SS_loc, "/")) {
    SS_loc <- paste0(SS_loc, "/")
  }

  # Get the executable path
  executable_path <- paste0(SS_loc,ss_exe)


  # determine the operating system
  if(.Platform[["OS.type"]] == "windows"){OS <- "windows"}
  if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="x86_64"){OS <- "Mac"}
  if(substr(R.version[["os"]], 1, 6) == "darwin" && R.version[["arch"]]=="aarch64"){OS <- "Mac"}
  if(R.version[["os"]] == "linux-gnu"){OS <- "linux"}

  # Check for whitespaces in executable path for Mac and Linux OS
  if(OS != "windows" & grepl("\\s", executable_path)) stop("Remove whitespaces from your SS file path you lunatic!")


  # if(do_hess){
  #   if(OS == "windows"){
  #     command <- paste0("C: && cd ",SS_dr,"&&",SS_loc,"/",ss_exe)
  #   } else{
  #     command <- paste0("chmod +x ", executable_path, " ; cd ", SS_dr, "; ../",ss_exe)
  #   }
  # } else{
  #   if(OS == "windows"){
  #     command <- paste0("C: && cd ",SS_dr,"&&",SS_loc,"/",ss_exe, " -nohess")
  #   } else{
  #     command <- paste0("chmod +x ", executable_path, " ; cd ", SS_dr, "; ../",ss_exe, " -nohess -nox")
  #   }
  # }

  # Run SS3 from within the SS directory
  # Calculate hessian matrix if specified

  if(OS == "windows"){
    command <- paste0("C: && cd ",SS_dr,"&&",SS_loc,"/",ss_exe)
  } else{
    command <- paste0("chmod +x ", executable_path, " ; cd ", SS_dr, "; ",executable_path)
  }

  if(do_hess){ paste0(command, " -nohess -nox")}


  if(print_updates_to_screen)
    cat(crayon::blue("\nRunning Stock Synthesis\n"))

  setwd(SS_dr)

  if(OS == "windows"){
    system("cmd.exe", input = command, intern=!print_r4SS_to_screen)
  } else {
    system(command, intern=!print_r4SS_to_screen)
  }

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



#' create_MG_Parms_df
#' @description Function to create the inputs$ctl$MG_parms table.The number and order of
#'     parameters depends on the number of growth patterns and areas. This
#'     function creates the table in the correct order, according to the number
#'     of GP and areas specified. Currently, movement isn't included. This
#'     needs to be added to the function
#'
#' @param N_growth_patterns Number of growth patterns used in the model
#' @param n_areas Number of areas in the model
#' @param GP_per_area Number of growth patterns that occur in an area
#'
#' @returns A data.frame formatted for inputs$ctl$MG_parms for `r4ss`
#' @export
#'
#' @examples create_MG_Parms_df( N_growth_patterns = 1,
#'  n_areas = 1,
#'  GP_per_area = 1 )
create_MG_Parms_df <- function(N_growth_patterns = 1, n_areas = 1, GP_per_area = 1){

  if(GP_per_area > N_growth_patterns) message("More GP per area specified than number of GP requested. Check outputs")

  Biopars <-c("NatM_p_1_SEX_GP_#",
              "L_at_Amin_SEX_GP_#",
              "L_at_Amax_SEX_GP_#",
              "VonBert_K_SEX_GP_#",
              "CV_young_SEX_GP_#",
              "CV_old_SEX_GP_#",
              "Wtlen_1_SEX_GP_#",
              "Wtlen_2_SEX_GP_#",
              "Mat50%_SEX_GP_#",
              "Mat_slope_SEX_GP_#",
              "Eggs_alpha_SEX_GP_#",
              "Eggs_beta_SEX_GP_#")

  PARROWS <- NULL
  for(sex in c("Fem", "Mal")){
    for(gp in 1:N_growth_patterns){
      tmpa <- gsub(pattern = "SEX", replacement = sex, Biopars)
      tmpb <- gsub(pattern = "#", replacement = gp, tmpa)

      if(sex == "Mal") tmpb <- tmpb[!grepl("Mat50%|Mat_slope|Eggs",tmpb)]
      PARROWS <- c(PARROWS, tmpb)
    }
  }

  if(n_areas > 1){
    if(GP_per_area == 1 & N_growth_patterns == 1 ){

      for(area in 1:n_areas){
        tmpc <- paste0("RecrDist_GP_1_area_",area,"_month_1")
        PARROWS <- c(PARROWS, tmpc)
      }
    }else if(GP_per_area == 1){
      for(area in 1:n_areas){
        tmpc <- paste0("RecrDist_GP_",area,"_area_",area,"_month_1")
        PARROWS <- c(PARROWS, tmpc)
      }
    }else {
      for(gp in 1:N_growth_patterns){
        for(area in 1:n_areas){
          tmpc <- paste0("RecrDist_GP_",gp,"_area_",area,"_month_1")
          PARROWS <- c(PARROWS, tmpc)
        }
      }
    }
  }
  PARROWS <- c(PARROWS, "CohortGrowDev")
  for(gp in 1:N_growth_patterns){
    tmpd <- paste0("FracFemale_GP_",gp)
    PARROWS <- c(PARROWS, tmpd)
  }

  PARCOLS <- c("LO","HI", "INIT", "PRIOR" , "PR_SD", "PR_type", "PHASE",
               "env_var&link", "dev_link", "dev_minyr", "dev_maxyr",
               "dev_PH", "Block", "Block_Fxn")

  df <-as.data.frame( matrix(ncol = 14,
                             nrow = length(PARROWS),
                             dimnames = list(PARROWS,
                                             PARCOLS)))

  return(df)

}

#' Tune_SS_Comps
#' @description Length and age compositions are tuned using either the Francis (default) or MI
#'     methods. This function is a wrapper around the `r4ss::tune_comps` function
#'     which 1) copies files to necessary locations with the correct permisions
#'     to use SS (see description for `Run_Stock_Synthesis`), and 2) will perform
#'     numerous iterations of the tuning in an attempt to achieve a stable tuned
#'     model. The user can specify the number of iterations to run or set
#'     `auto_run == TRUE` so that the model is tuned until criteria are met. These
#'     include 1) changes in total log likelihood are less than 2, and 2) the model
#'     has evidence of convergence.
#' @param Origin The working directory on a network or hard drive where the
#'     Stock Synthesis assessment is being performed. The SS files will be copied
#'     from here to `SS_dr` where the model is run. If `copy_to_origin == TRUE`
#'     (default) then the SS model files are copied back to this directory.
#' @param SS_dr The location on the hard drive where the SS model will be run locally.
#'     make sure there is no white space in this filepath, especially if using a
#'     non Windows computer.
#' @param SS_loc The location of the SS executable file.
#' @param ss_exe The name of the SS executable. The default is 'ss3_osx_arm64'
#'     which corresponds to my own operating system.
#' @param over_write_model Should the existing tuned model be overwritten. `FALSE`
#'.    will generate a copy of the model to be tuned. `TRUE` will overwrite the
#'.    existing tuned model (necessary for iterative tuning steps)
#' @param method `Francis` or `MI` for McCallister and Ianelli method
#' @param copy_to_origin Do you want the SS files (and `r4ss` plots if requested)
#'     copied back to the `Origin`? This can take a while if working remotely with
#'     an agency network.
#' @param auto_run Keep running tuning until further iterations no longer improve
#'.    the model. Maxes out at 50 runs
#' @param n_iters Pre-set number of tuning iterations. Ignored if `auto-run == TRUE`
#' @param plots Do you want to produce the `r4ss` outputs?
#' @param ... Function calls to be passed to `r4ss`
#'
#' @returns A tuned SS model following the instructions given. No R objects are returned.
#' @export
#'
Tune_SS_Comps <- function(
    Origin = NULL,
    SS_dr = NULL,
    SS_loc = '/Users/jonathansmart/Documents/SS/',
    ss_exe = "ss3_osx_arm64",
    over_write_model = FALSE,
    method = "Francis",
    copy_to_origin = TRUE,
    auto_run = FALSE,
    n_iters = NULL, # number of iterations to run
    plots = TRUE,
    ...
){

  if(!method %in% c("Francis", "MI")) stop("'method' should be either 'Francis' or 'MI' for the use of the 'Tune_SS_Comps' function")

  if(is.null(Origin)) stop("SS directory Network not specified")
  if(is.null(SS_dr)) stop("SS directory on C drive not specified")
  if(!is.null(n_iters) & auto_run == TRUE) warning("n_iters will be ignored as auto_run == TRUE")

  if(is.null(n_iters) & auto_run == FALSE) stop("n_iters must be specified if auto_run == FALSE")

  # Should the files be written to a new folder or over write existing model.
  if(over_write_model == FALSE){
    new_path <- paste0(Origin,"_Tuned")

    # Copy files to new directory
    r4ss::copy_SS_inputs(dir.old = Origin, dir.new = new_path,
                         overwrite = T,
                         verbose = TRUE
    )

    new_c_drive <- paste0(SS_dr,"_Tuned")
  } else {
    new_path <- Origin
    new_c_drive <- SS_dr
  }

  cat(crayon::blue("\nRunning un-tuned SS model\n"))
  # Run SS model to get initial un-tuned model
  Run_Stock_Synthesis(Origin = new_path, # network file path with files to use
                      SS_dr = new_c_drive, # The new directory on C drive that outputs will get written to
                      SS_loc = SS_loc, #location of ss3.exe
                      print_r4SS_to_screen = FALSE,  # how much output do you want vomited back at you?
                      print_updates_to_screen = FALSE,
                      do_hess = TRUE, # estimate hessian?
                      Ask = FALSE,
                      plots = FALSE, # run r4ss plots?
                      copy_to_origin = FALSE)

  if(!file.exists(file.path(SS_dr,"Report.sso"))) stop("Un-tuned version of Stock Synthesis has not run. Check input files")

  cat(paste(crayon::green("\n\u2713"),crayon::blue("Un-tuned SS has run\n")))

  model_outputs <- suppressMessages(r4ss::SS_output(dir = SS_drive, printstats = F))

  Outputs <- data.frame(Values = round(c(model_outputs$likelihoods_used[,"values"],
                                         model_outputs$current_depletion,
                                         model_outputs$SBzero
  ),3))

  if(auto_run == FALSE){
    Results <- as.data.frame(matrix(nrow = nrow(Outputs), ncol = n_iters+1))

    rownames(Results) <- c(rownames(model_outputs$likelihoods_used),"Bfinal", "B0")
    colnames(Results) <- c("Un-tuned",paste("Iter",1:n_iters))
    Results[,1] <- Outputs[,1]


    for( i in 1:n_iters){
      # Load untuned SS inputs and update with recomended tuning settings

      inputs <- r4ss::SS_read(dir = new_c_drive, verbose = FALSE)

      # Get suggested Variance adjustment factors
      varadjust <- suppressMessages(
        suppressWarnings(
          r4ss::tune_comps(dir = new_c_drive, option = method,
                           verbose = FALSE)
        )
      )

      # Turn variance adjustment on
      inputs$ctl$DoVar_adjust <- 1

      # Provide the suggested variance adjustments to the control file
      inputs$ctl$Variance_adjustment_list <- varadjust[,1:3]

      # Update column names
      colnames(inputs$ctl$Variance_adjustment_list) <- c("Data_type", "Fleet", "Value")

      # Update row names according to var adjust type (length or age) and fleet name
      rownames(inputs$ctl$Variance_adjustment_list) <- paste0(ifelse(inputs$ctl$Variance_adjustment_list$Data_type == 4,"LenComp", "AgeComp"),"_VarAdjust_",
                                                              inputs$dat$fleetnames[inputs$ctl$Variance_adjustment_list$Fleet],
                                                              "(",inputs$ctl$Variance_adjustment_list$Fleet,")")

      # Overwrite files with tuning info
      r4ss::SS_write(inputs, dir =  new_c_drive, overwrite = TRUE, verbose = FALSE)
      # SS_write(inputs, dir =  new_path, overwrite = TRUE, verbose = FALSE)


      cat(paste0(crayon::blue("\nRunning comp tuning for iteration ",i,"/",n_iters,"\n")))

      suppressMessages(
        Run_Stock_Synthesis(Origin = new_c_drive, # network file path with files to use
                            SS_dr = new_c_drive, # The new directory on C drive that outputs will get written to
                            SS_loc = SS_loc, #location of ss3.exe
                            print_r4SS_to_screen = FALSE,  # how much output do you want vomited back at you?
                            print_updates_to_screen = FALSE,
                            do_hess = TRUE, # estimate hessian?
                            Ask = FALSE,
                            plots = ifelse(i == n_iters, plots, FALSE), # run r4ss plots?
                            copy_to_origin =  FALSE,
                            ...) # Add Blim to r4ss plots
      )
      model_outputs <- suppressMessages(r4ss::SS_output(dir = new_c_drive, printstats = F))

      Outputs <- data.frame(Values = round(c(model_outputs$likelihoods_used[,"values"],
                                             model_outputs$current_depletion,
                                             model_outputs$SBzero
      ),3))

      Results[,1+i] <- Outputs[,1]

      print(Results[1:nrow(Results),1:(1+i)])

      cat(paste(crayon::green("\n\u2713"),paste0(crayon::blue("SS has run for iteration",i,"/",n_iters,"\n"))))
    }
  }

  if(auto_run == TRUE){

    Results <- as.data.frame(matrix(nrow = nrow(Outputs), ncol = 51))

    rownames(Results) <- c(rownames(model_outputs$likelihoods_used),"Bfinal", "B0")
    colnames(Results) <- c("Un-tuned",paste("Iter",1:50))
    Results[,1] <- Outputs[,1]
    i = 1
    converge <- FALSE
    while(converge == FALSE){
      # Load untuned SS inputs and update with recomended tuning settings

      inputs <- r4ss::SS_read(dir = new_c_drive, verbose = FALSE)

      # Get suggested Variance adjustment factors
      varadjust <- suppressMessages(
        suppressWarnings(
          r4ss::tune_comps(dir = new_c_drive, option = method,
                           verbose = FALSE)
        )
      )

      # Turn variance adjustment on
      inputs$ctl$DoVar_adjust <- 1

      # Provide the suggested variance adjustments to the control file
      inputs$ctl$Variance_adjustment_list <- varadjust[,1:3]

      # Update column names
      colnames(inputs$ctl$Variance_adjustment_list) <- c("Data_type", "Fleet", "Value")

      # Update row names according to var adjust type (length or age) and fleet name
      rownames(inputs$ctl$Variance_adjustment_list) <- paste0(ifelse(inputs$ctl$Variance_adjustment_list$Data_type == 4,"LenComp", "AgeComp"),"_VarAdjust_",
                                                              inputs$dat$fleetnames[inputs$ctl$Variance_adjustment_list$Fleet],
                                                              "(",inputs$ctl$Variance_adjustment_list$Fleet,")")

      # Overwrite files with tuning info
      r4ss::SS_write(inputs, dir =  new_c_drive, overwrite = TRUE, verbose = FALSE)
      # SS_write(inputs, dir =  new_path, overwrite = TRUE, verbose = FALSE)


      cat(paste0(crayon::blue("\nRunning comp tuning for iteration ",i,"\n")))

      suppressMessages(
        Run_Stock_Synthesis(Origin = new_c_drive, # network file path with files to use
                            SS_dr = new_c_drive, # The new directory on C drive that outputs will get written to
                            SS_loc = SS_loc, #location of ss3.exe
                            print_r4SS_to_screen = FALSE,  # how much output do you want vomited back at you?
                            print_updates_to_screen = FALSE,
                            do_hess = TRUE, # estimate hessian?
                            Ask = FALSE,
                            plots = FALSE, # run r4ss plots?
                            copy_to_origin =  FALSE,
        )#...) # Add Blim of 0.2 to r4ss plots
      )
      model_outputs <- suppressMessages(SS_output(dir = new_c_drive,printstats = F))

      Outputs <- data.frame(Values = round(c(model_outputs$likelihoods_used[,"values"],
                                             model_outputs$current_depletion,
                                             model_outputs$SBzero
      ),3))

      i = i + 1

      if(i >= 51) {
        stop("Fifty model runs have been performed without achieving stable model estimates.")
      }

      Results[,i] <- Outputs[,1]

      print(Results[1:nrow(Results),1:(i)])

      cat(paste(crayon::green("\n\u2713"),paste0(crayon::blue("SS has run for iteration",i-1,"\n"))))

      # converge <- ifelse(i > 2 & all(c(Results[1,i]-abs(Results[1,i-1]) < 2 , Results[1,i-1]-abs(Results[1,i-2]) < 2)), TRUE, FALSE)

      if(i >= 4){

        Cond_1 <- abs(Results[1,i]-Results[1,i-1]) < 2 # log lik diff of less than two for model runs?
        Cond_2 <- abs(Results[1,i-1]-Results[1,i-2]) < 2# log lik diff of less than two for model runs?
        converge <- all(Cond_1, Cond_2) # multiplr model runs with og lik diff of less than two

      }

    }

    if(plots) {
      suppressMessages( r4ss::SS_plots(model_outputs, verbose = FALSE, ...))
    }

  }

  #Write input files back to Network for even if copy_to_origin is FALSE
  if(copy_to_origin){

    cat(crayon::blue("\nSS files are being copied to network\n"))
    New_SS_files <- list.files(new_c_drive, full.names = TRUE)[grep(".exe",invert = T, list.files(new_c_drive, full.names = TRUE))]
    # Copy files back to the original folder
    file.copy(New_SS_files, new_path, overwrite = TRUE, recursive = T)
    # cat("\n\u2713 File transfer complete\n")
    cat(paste(crayon::green("\n\u2713"),crayon::blue("File transfer complete\n")))

  } else{
    r4ss::SS_write(inputs, dir =  new_path, overwrite = TRUE, verbose = FALSE)
  }

  if(auto_run == FALSE)
    message("Likelihoods in later iterations should be similar to indicate tuning is no longer improving fit\n set `auto_run == TRUE` to run as many iterations as necessary to reach this criteria")


  Results <- Results[,colSums(is.na(Results))<nrow(Results)]

  return(Results)
}


#' Profile_SS_Parameters
#' @description This is a wrapper function around `r4ss::profile` which, again,
#'     runs stock synthesis from the designated location on your computer and stores
#'     the outputs in the directory that the stock assessment is being developed.
#'     The profile plots for the chosen parameter are automatically saved in new
#'     locations on the computer.
#' @param Origin The working directory on a network or hard drive where the
#'     Stock Synthesis assessment is being performed. The SS files will be copied
#'     from here to `SS_dr` where the model is run. If `copy_to_origin == TRUE`
#'     (default) then the SS model files are copied back to this directory.
#' @param SS_dr The location on the hard drive where the SS model will be run locally.
#'     make sure there is no white space in this filepath, especially if using a
#'     non Windows computer.
#' @param SS_loc The location of the SS executable file.
#' @param string A string that determines which parameter within the model is
#'     profiled.
#' @param vector The vector of parameter values to be profiled
#' @param Piner Should Piner plots be produced
#' @param Comparisons Should model comparison plots be produced?
#' @param profile.label Label for the plot titles (typically the parameter being profiled)
#' @param legendlabels Labels used for the plot legends
#' @param ss_exe The name of the SS executable. The default is 'ss3_osx_arm64'
#'     which corresponds to my own operating system.
#' @param print_r4SS_to_screen How much information from `r4ss` would you like
#'     vommitted back at you?
#' @param copy_to_origin Do you want the SS files (and `r4ss` plots if requested)
#'     copied back to the `Origin`? This can take a while if working remotely with
#'     an agency network.
#'
#' @returns Plots saved to created directories of the parameter profiling. No
#'     R objects are returned.
#' @export
#'
Profile_SS_Parameters <- function(Origin = getwd(), # Folder on network drive with the model to be profiled
                                  SS_dr = NULL, # top level folder where models are being run on C drive
                                  string = NULL, # a string that will select the parameter to be profiled ("steep", "NatM_p_1_Fem_GP_1", etc.). Does not need to match exactly but benefits from this
                                  vector = NULL, # The vector to profile over (seq(0.3, 0.90, 0.1))
                                  Piner = TRUE, # Print the Piner plot?
                                  Comparisons = TRUE, # Print the comparison plots? There are a few of these
                                  profile.label = NULL, # Lable for the Piner plot
                                  legendlabels = NULL, # Legend labels for comparison plots
                                  SS_loc = '/Users/jonathansmart/Documents/SS/',
                                  ss_exe = "ss3_osx_arm64",
                                  print_r4SS_to_screen = FALSE, # How much output do you want vomitted back at you?
                                  copy_to_origin = TRUE # Copy the files back to the network?
){




  if(is.null(Origin)) stop("SS directory Network not specified")
  if(is.null(SS_dr)) stop("SS directory on C drive not specified")


  # set path for profile on C drive
  dir_prof <- file.path(SS_dr,"profile",string)

  # Clean folder of old run
  Old_SS_files <- list.files(dir_prof, full.names = TRUE)
  unlink(Old_SS_files, recursive = TRUE)



  # Copy files to new profile directory
  r4ss::copy_SS_inputs(
    dir.old = Origin,
    dir.new = dir_prof,
    create.dir = TRUE,
    recursive = TRUE,
    overwrite = TRUE,
    copy_par = TRUE,
    verbose = print_r4SS_to_screen
  )

  # Setup starter file for profiling
  starter <- r4ss::SS_readstarter(file.path(dir_prof, "starter.ss"),verbose = print_r4SS_to_screen)
  starter[["ctlfile"]] <- "control_modified.ss"
  starter[["prior_like"]] <- 1
  r4ss::SS_writestarter(starter, dir = dir_prof, overwrite = TRUE, verbose = print_r4SS_to_screen)

  # Run profiling

  Nprofile <- length(vector)
  # run profile command

  cat(crayon::blue("\nFile structure is setup. Now running profiles\n"))

  prof.table <- r4ss::profile(
    dir = dir_prof,
    oldctlfile = "control.ss",
    newctlfile = "control_modified.ss",
    string = string, # subset of parameter label
    profilevec = vector,
    verbose = print_r4SS_to_screen,
    exe =  paste0(SS_loc,ss_exe)
  )



  # read the output files (with names like Report1.sso, Report2.sso, etc.)
  profilemodels <- r4ss::SSgetoutput(dirvec = dir_prof, keyvec = 1:Nprofile,
                               verbose = print_r4SS_to_screen)
  # summarize output
  profilesummary <- r4ss::SSsummarize(profilemodels,
                                verbose = print_r4SS_to_screen)

  cat(paste(crayon::green("\n\u2713"),crayon::blue("Profiling complete\n")))

  # Create directory for plots
  plot_dir <- paste0(dir_prof,"/plots")


  if(Piner){

    # Correction for M parameter
    if(string == 'NatM_p_1_Fem_GP_1') string <- "NatM_uniform_Fem_GP_1"

    # plot profile using summary created above
    r4ss::SSplotProfile(profilesummary, # summary object
                  profile.string = string, # substring of profile parameter
                  profile.label = profile.label,
                  plotdir = plot_dir,
                  add_cutoff = TRUE,
                  verbose = print_r4SS_to_screen,
                  print = TRUE)

    cat(paste(crayon::green("\n\u2713"),crayon::blue("Piner plot produced\n")))

  }

  if(Comparisons){
    # make timeseries plots comparing models in profile
    suppressMessages(
      r4ss::SSplotComparisons(profilesummary,
                        legendlabels = legendlabels,
                        plotdir = plot_dir,
                        verbose = print_r4SS_to_screen,
                        print = TRUE)
    )
    cat(paste(crayon::green("\n\u2713"),crayon::blue("Comparison plots produced\n")))

  }

  if(copy_to_origin){
    cat(crayon::blue("\nSS files are being copied to network\n"))
    New_SS_files <- list.files(dir_prof, full.names = TRUE)
    # Copy files back to the original folder

    if(!dir.exists(file.path(Origin,"profile", string))){
      dir.create(file.path(Origin,"profile", string),recursive = TRUE)
      if(dir.exists(file.path(Origin,"profile", string)))
        cat(paste(crayon::green("\n\u2713"),crayon::blue(paste(file.path(Origin,"profile", string),"has been created\n"))))
    }
    file.copy(New_SS_files, file.path(Origin,"profile", string), overwrite = TRUE, recursive = T)
    # cat("\n\u2713 File transfer complete\n")
    cat(paste(crayon::green("\n\u2713"),crayon::blue("File transfer complete\n")))
  }



}



