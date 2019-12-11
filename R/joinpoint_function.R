# set up data ----
rm(list=setdiff(ls(), "round2"))
library(dplyr)
library(glue)
library(data.table)

dt <- data.table(
  indic = c(rep("my.indicator", 14)), 
  group1 = c(rep("group1", 14)), 
  group2 = c(rep("group2.A", 7), rep("group2.B", 7)),
  time = c(rep(1:7, 2)), 
  est = c(.11, .12, .13, .14, .15, .16, .17, .29, .48, .67, .32, .12, .17, .55),
  se = c(runif(14, .01, .02))
  )

# JoinPoint function ----
jp_f <- function(jp_data = NULL,
                 jp_indicator = NULL,
                 jp_period = "chi_year",
                 jp_result = "result",
                 jp_se = "se",
                 jp_byvar1 = "cat1_group",
                 jp_byvar2 = "cat2_group",
                 jp_dir = NULL,
                 jp_path = "C:/Program Files (x86)/Joinpoint Command/jpCommand.exe") {
  
  ##############################################
  ## Confirm function arguments are logical ####
  ##############################################
    # confirm provision of jp_data
      if(is.null(jp_data)){
        stop("You must specify a dataset (i.e., 'jp_data' must be defined)")
      }
  
   # confirm provision of jp_indicator
    if(is.null(jp_indicator)){
      stop("You must specify an indicator column (i.e., 'jp_indicator' must be defined)")
    }
  
  # confirm that jp_indicator exists
    if(!jp_indicator %in% names(jp_data)){
      stop(glue("<{jp_indicator}> does not exist in your specified dataset. 
                  'jp_indicator' must refer to a column containing your indicator of interest."))
    }

  # confirm that jp_period exists
    if(!jp_period %in% names(jp_data)){
      stop(glue("<{jp_period}> does not exist in your specified dataset. 
                'jp_period' must refer to a column measuring time in your dataset."))
    }
  
  # confirm that jp_result exists
    if(!jp_result %in% names(jp_data)){
      stop(glue("<{jp_result}> does not exist in your specified dataset. 
                  'jp_result' must refer to a column containing your estimtates/results."))
    }
  
  # confirm that jp_se exists
    if(!jp_se %in% names(jp_data)){
      stop(glue("<{jp_se}> does not exist in your specified dataset. 
                  'jp_se' must refer to a column containing your standard error."))
    }
  
  # confirm that jp_byvar1 exists
    if(!jp_byvar1 %in% names(jp_data)){
      stop(glue("<{jp_byvar1}> does not exist in your specified dataset. 
                'jp_byvar1' must refer to a column containing your first grouping / by variable."))
    }
  
  # confirm that jp_byvar2 exists
    if(!jp_byvar2 %in% names(jp_data)){
      stop(glue("<{jp_byvar2}> does not exist in your specified dataset. 
                  'jp_byvar2' must refer to a column containing your second grouping / by variable."))
    }
  
  # confirm provision of jp_dir
    if(is.null(jp_dir)){
      stop("You must specify a directory for JoinPoint results to be saved (i.e., 'jp_dir' must be defined)")
    }
  
  # confirm jp_path exists
    if(!file.exists(jp_path)){
      stop(glue("The file path to the JoinPoint executable specified by 'jp_path' does not exist.
                I.e., {jp_path} does not exist. 
                If you have installed joinpoint, please specify the proper filepath, 
                otherwise please install the program before continuing."))
    }
  
  ############################
  ##          Set up      ####
  ############################  
  
    # Set up varname ----
        indicator <- unique(jp_data[, get(jp_indicator)])
        if (length(indicator) != 1) {
          stop ("You must specify one and only one 'jp_indicator'")
        }
  
    # Create jp_dir folder (if needed) ----
        if(!dir.exists(jp_dir)){
          message(glue("'{jp_dir}' does not exist and is being created on your behalf"))
          dir.create(jp_dir)
          }
  
    # Create input & output folder in jp_dir (if needed) ----
        if(!dir.exists(glue("{jp_dir}/input"))){dir.create(glue("{jp_dir}/input"))}
        if(!dir.exists(glue("{jp_dir}/output"))){dir.create(glue("{jp_dir}/output"))}
        

    # Write tab seperated input data file for use by JoinPoint ----
        # Note: outputting an integer seems to cause JP to fail. 
        # Check for integers then subtract/add a tiny extra amount to avoid errors
        input <- copy(jp_data)
        for(i in c("indicator", "period", "result", "se", "byvar1", "byvar2")){
          input[, paste0("jp_", i) := get(get(paste0("jp_", i)))] # ascribe standard names for simplicity
          input[, get(paste0("jp_", i)) := NULL]
        }
        input[, jp_result := as.numeric(jp_result)]
        input[jp_result == round2(jp_result) & jp_result==1, jp_result := jp_result - 0.0001] # if integer & == 1, then subtract tiny num to keep %in% [0,1]
        input[jp_result == round2(jp_result) & jp_result!=1, jp_result := jp_result + 0.0001] # if integer & !=1, then add tiny number
        input[jp_se == 0, jp_se := NA_real_][, jp_se2 := mean(jp_se, na.rm = T), by = c("jp_byvar2")][is.na(jp_se), jp_se := jp_se2][, jp_se2 := NULL]# when se is zero, replace with mean of when it is not zero
        input[jp_se == round2(jp_se), jp_se2 := mean(input[jp_se != 0]$jp_se, na.rm = T), by = c("jp_byvar1", "jp_byvar2")] # when se is any integer, replace with mean of when it is not zero
        input <- input[, .(jp_period, jp_byvar1, jp_byvar2, jp_result, jp_se)] # limit to columns needed for JoinPoint
        setorder(input, jp_byvar1, jp_byvar2, jp_period)
        
        write.table(input, file = glue(jp_dir, "/input/JP_{unlist(indicator)}.txt"),
                    row.names = F, col.names = F, sep = "\t")
        
        
    # Track the integers that JoinPoint creates corresponding to category names ----
        input_names_jp_byvar1 <- input %>% distinct(jp_byvar1) %>%
          mutate(jp_byvar1_num = seq(0, n_distinct(input$jp_byvar1) - 1))
        
        input_names_jp_byvar2 <- input %>% distinct(jp_byvar2) %>%
          mutate(jp_byvar2_num = seq(0, n_distinct(input$jp_byvar2) - 1))
        
        input <- left_join(input, input_names_jp_byvar1, by = "jp_byvar1") %>%
          left_join(., input_names_jp_byvar2, by = "jp_byvar2")
    
  
    # Create an Run.ini file for Joinpoint ----
        cat(file = file.path(jp_dir, "Run.ini"),
            glue("
                     [Joinpoint Input Files]
                     Session File={jp_dir}/Session.ini
                     Run Options File={jp_dir}/Options.ini
                     Export Options File={jp_dir}/Options.ini
                     Output File={jp_dir}/output/{unlist(indicator)}.jpo"),
            append = F)
  
    # Create a Session.ini file ----
        cat(file = file.path(jp_dir, "Session.ini"),
            glue("
                 [Datafile options]
                 Datafile name={jp_dir}/input/JP_{unlist(indicator)}.txt
                 File format=DOS/Windows
                 Field delimiter=tab
                 Missing character=space
                 Fields with delimiter in quotes=true
                 Variable names include=true
                 
                 [Joinpoint Session Parameters]
                 independent variable=jp_period
                 independent variable location=1
                 by-var1=jp_byvar1
                 by-var1 location=2
                 by-var2=jp_byvar2
                 by-var2 location=3
                 age-adjusted rate=jp_result
                 age-adjusted rate location=4
                 proportion=jp_result
                 proportion location=4
                 standard error=jp_se
                 standard error location=5"),
            append = F)
        
    # Create an Options.ini file if needed ----
        if(file.exists(glue("{jp_dir}/Options.ini")) != TRUE){
          cat(file = file.path(jp_dir, "Options.ini"),
              glue("          
                    [Session Options]
                    Model=ln
                    Model selection method=permutation test
                    Permutations significance level=0.05
                    Num permutations=4499
                    Method=grid
                    Min obs end=2
                    Min obs between=2
                    Num obs between=0
                    CI method=parametric
                    Autocorr errors=0
                    Het error=standard error
                    Het error variable location=2
                    Random number generator seed=98104

                    [Export Options]
                    Models=best fit
                    Line delimiter=unix
                    Missing character=space
                    Field delimiter=tab
                    Output by-group headers=false
                    By-var format=numeric
                    APC Precision=1
                    AAPC Precision=1
                    X Values Precision=5
                    Y Values Precision=9
                    Remove best fit flags=true
                    AAPC Last Obs=true
                    Estimated Joinpoints Precision=6
                    estimated joinpoints precision=6"),
          append = F)
        }

  ############################
  ##          Run         ####
  ############################
    # Set file path for Run.ini created above ----
      argument <- glue('"{jp_dir}/Run.ini"') 

    # Submit data and analysis details to JoinPoint ----
      system2(eval(jp_path), args = argument, stdout=TRUE, stderr=TRUE)
  
      # If there was an error, run it up to 10 times ... until it works! 
      # this is necessary because the program is buggy. 
        i<-1
        while(
          i <= 10 &
          (file.exists(paste0(jp_dir, "Run.ErrorFile.txt")) == T |
           file.exists(glue(jp_dir, "/output/{unlist(indicator)}.aapcexport.txt")) == F)
          ){
          unlink(paste0(jp_dir, "Run.ErrorFile.txt"))
          system2(eval(jp_path), args = argument, stdout=TRUE, stderr=TRUE) # run again if have error
          i <- i + 1
          Sys.sleep(.5)
        }
        
    # Process JoinPoint Results or errors ----
        if(i > 10){
          stop(glue("JoinPoint run failed ... please run the entire JoinPoint function again.
                    Ten failed attempts were made to calculate the trend for {indicator}.
                    Please double check your input data in {jp_dir}/input/JP_{unlist(indicator)}.txt"))
        } else {
          # Bring in the annual percent change
          # (assumes output was tab deliminated, set this in options.ini file )
          temp_trend <- fread(glue("{jp_dir}/output/{unlist(indicator)}.apcexport.txt")) # apc = annual percent change
          setnames(temp_trend, names(temp_trend), gsub(" ", ".", names(temp_trend)))
          
          # save trend data for each time period
          temp_trend[, trend := "flat"] # set flat as default, and then specify when rising or falling
          temp_trend[APC.Significant==1 & APC>0, trend := "rising"]
          temp_trend[APC.Significant==1 & APC<0, trend := "falling"]
          temp_trend <- temp_trend[, .(jp_byvar1, jp_byvar2, Segment.Start, Segment.End, trend)] # keeping only essential vars, helpful for clarity while coding/troubleshooting
          
          # collapse data when two contiguous time periods have the same trend (i.e., 2005-2009 and 2009-2013 both flat, so 2005-2013 is flat)
          temp_trend[, trend.next := shift(trend, 1, 0, "lead"), by = .(jp_byvar1, jp_byvar2)]
          temp_trend[, trend.prev := shift(trend, 1, 0, "lag"), by = .(jp_byvar1, jp_byvar2)]
          temp_trend[trend == trend.next | trend == trend.prev, contiguous := TRUE][, c("trend.next", "trend.prev") := NULL] # identify contiguous
          
          temp_trend <- rbind(temp_trend[contiguous==TRUE, .(Segment.Start = min(Segment.Start), Segment.End = max(Segment.End)), 
                                         by = .(jp_byvar1, jp_byvar2, trend, contiguous)], 
                              temp_trend[is.na(contiguous), ]
          ) # collapse if congtiguous and append to non-contiguous
          
          temp_trend[, contiguous := NULL] # drop contiguous indicator
          
          setorder(temp_trend, jp_byvar1, jp_byvar2, Segment.Start)
          
          # create formatted trend summary
          if(min(nchar(temp_trend$Segment.Start), na.rm = T) == 4 & max(nchar(temp_trend$Segment.End), na.rm = T) == 4){
            temp_trend[, trend := paste0(Segment.Start, "-", substr(Segment.End, 3, 4), ": ", trend)] # use for 4 digit years (i.e., 2007-11:)
          } else {
            temp_trend[, trend := paste0(Segment.Start, "-", Segment.End, ": ", trend)]
          }

          # drop start and end columns      
          temp_trend[, c("Segment.Start", "Segment.End") := NULL]
          
          # create a counter for each group.num
          temp_trend[,  count := 1:.N, by = .(jp_byvar1, jp_byvar2)]
          
          # reshape from long to wide
          temp_trend <- dcast(temp_trend, jp_byvar1 + jp_byvar2 ~ count, value.var = "trend")
          
          # append the trends across each row
          cols <- names(temp_trend)[3:ncol(temp_trend)]
          temp_trend[, time_trends:= apply(.SD, 1, function(x) paste(x, collapse = '; ')), .SDcols = cols]
          temp_trend[, time_trends := gsub("; NA", "", time_trends)]
          
          # keep columns of interest
          temp_trend <- temp_trend[, .(jp_byvar1, jp_byvar2, time_trends)]
          
          
          ### Join back to data and make trends tab for joining
          output <- left_join(input, temp_trend,
                              by = c("jp_byvar1_num" = "jp_byvar1", "jp_byvar2_num"= "jp_byvar2")) %>%
            mutate(tab = "trends") %>%
            distinct(tab, jp_byvar1, jp_byvar2, time_trends)# Only need to keep one row per jp_byvar2 since they're all the same
          
          
          ### Remove unwanted files made by JoinPoint ----???
          toss.results <- list(c(glue('{jp_dir}/Run.jps'),
                                 glue('{jp_dir}/Run.ErrorFile.txt'),
                                 glue('{jp_dir}/output/{unlist(indicator)}.aapcexport.txt'),
                                 glue('{jp_dir}/output/{unlist(indicator)}.apcexport.txt'),
                                 glue('{jp_dir}/output/{unlist(indicator)}.dataexport.txt'),
                                 glue('{jp_dir}/output/{unlist(indicator)}.permtestexport.txt'),
                                 glue('{jp_dir}/output/{unlist(indicator)}.report.txt'),
                                 glue('{jp_dir}/output/{unlist(indicator)}.RunSummary.txt')))
          
          do.call(unlink, toss.results)
          
          return(output)
        }
}


