# JoinPoint function ----
jp_f <- function(data, jp_dir = NULL) {
  # Set up varname ----
  indicator <- distinct(data, indicator_key)
  if (length(indicator) != 1) {
    stop ("# indicators != 1 in the data")
  }
  
  # Check jp_dir exists ----
  if (is.null(jp_dir)) {
    stop("Specify the folder to store JP input and output")
  }
  
  # Check that output subfolder exists (otherwise JP throws an error) ----
  if (dir.exists(glue("{jp_dir}/output")) == F) {
    dir.create(glue("{jp_dir}/output"))
  }
  
  # Write Output JP file ----
  # Note: outputting an integer seems to cause JP to fail. Check for integers
  # then subtract/add a tiny extra amount.
  output <- copy(data)
  output[, year := as.numeric(year)]
  output[result == round2(result) & result==1, result := result - 0.0001] # if integer & == 1, then subtract tiny num to keep %in% [0,1]
  output[result == round2(result) & result!=1, result := result + 0.0001] # if integer & !=1, then add tiny number
  output[se == 0, se := NA_real_][, se2 := mean(se, na.rm = T), by = c("cat2_group")][is.na(se), se := se2][, se2 := NULL]# when se is zero or any integer, replace with mean of when it is not zero
  output[se == round2(se), se2 := mean(output[se != 0]$se, na.rm = T), by = c("cat1_group", "cat2_group")] # when se is zero or any integer, replace with mean of when it is not zero
  output <- output[, .(year, cat1_group, cat2_group, result, se)] # limit to columns needed for JoinPoint
  setorder(data, cat1_group, cat2_group, year)
  
  write.table(output, file = glue(jp_dir, "/JP_{unlist(indicator)}.txt"),
              row.names = F, col.names = F, sep = "\t")
  
  # Track what JP converts the category names to ----
  output_names_cat1 <- output %>% distinct(cat1_group) %>%
    mutate(cat1_num = seq(0, n_distinct(output$cat1_group) - 1))
  output_names_cat2_group <- output %>% distinct(cat2_group) %>%
    mutate(cat2_group_num = seq(0, n_distinct(output$cat2_group) - 1))
  
  output <- left_join(output, output_names_cat1, by = "cat1_group") %>%
    left_join(., output_names_cat2_group, by = "cat2_group")
  
  
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
               Datafile name={jp_dir}/JP_{unlist(indicator)}.txt
               File format=DOS/Windows
               Field delimiter=tab
               Missing character=space
               Fields with delimiter in quotes=true
               Variable names include=true
               
               [Joinpoint Session Parameters]
               by-var1=cat1_group
               by-var1 location=2
               by-var2=cat2_group
               by-var2 location=3
               independent variable=year
               independent variable location=1
               age-adjusted rate=result
               age-adjusted rate location=4
               proportion=result
               proportion location=4
               standard error=se
               standard error location=5"),
      append = F)
  
  
  # Run JoinPoint ----
  argument <- glue('"{jp_dir}/Run.ini"') # filepath for the Run.ini file created above
  jp_path <- "C:/Program Files (x86)/Joinpoint Command/jpCommand.exe" # filepath for the JoinPoint program
  
  system2(eval(jp_path), args = argument, stdout=TRUE, stderr=TRUE)
  
  # If there was an error, run it again and again and again ... until it works! 
  # this is necessary because the program is buggy. 
  while(file.exists(paste0(jp_dir, "Run.ErrorFile.txt"))){
    unlink(paste0(jp_dir, "Run.ErrorFile.txt"))
    system2(eval(jp_path), args = argument, stdout=TRUE, stderr=TRUE) # run again if have error
    Sys.sleep(.5)
  }
  
  while(file.exists(glue(jp_dir, "/output/{unlist(indicator)}.aapcexport.txt")) == F){
    system2(eval(jp_path), args = argument, stdout=TRUE, stderr=TRUE) # run yet again if did not produce output file needed
    Sys.sleep(.5)
  }
  
  if (file.exists(glue(jp_dir, "/output/{unlist(indicator)}.aapcexport.txt")) == F | 
      file.exists(paste0(jp_dir, "Run.ErrorFile.txt"))) {
    stop("JoinPoint run failed ... please run the entire script again") # should not be necessary, but JoinPoint is shockingly bad, so who knows
  } else {
    # Bring in the annual percent change
    # (assumes output was tab deliminated, set this in options.ini file)
    temp_trend <- fread(glue("{jp_dir}/output/{unlist(indicator)}.apcexport.txt")) # apc = annual percent change
    setnames(temp_trend, names(temp_trend), gsub(" ", ".", names(temp_trend)))
    
    # save trend data for each time period
    temp_trend[, trend := "flat"] # set flat as default, and then specify when rising or falling
    temp_trend[APC.Significant==1 & APC>0, trend := "rising"]
    temp_trend[APC.Significant==1 & APC<0, trend := "falling"]
    temp_trend <- temp_trend[, .(cat1_group, cat2_group, Segment.Start, Segment.End, trend)] # keeping only essential vars, helpful for clarity while coding/troubleshooting
    
    # collapse data when two contiguous time periods have the same trend (i.e., 2005-2009 and 2009-2013 both flat, so 2005-2013 is flat)
    temp_trend[, trend.next := shift(trend, 1, 0, "lead"), by = .(cat1_group, cat2_group)]
    temp_trend[, trend.prev := shift(trend, 1, 0, "lag"), by = .(cat1_group, cat2_group)]
    temp_trend[trend == trend.next | trend == trend.prev, contiguous := TRUE][, c("trend.next", "trend.prev") := NULL] # identify contiguous
    
    temp_trend <- rbind(temp_trend[contiguous==TRUE, .(Segment.Start = min(Segment.Start), Segment.End = max(Segment.End)), 
                                   by = .(cat1_group, cat2_group, trend, contiguous)], 
                        temp_trend[is.na(contiguous), ]
    ) # collapse if congtiguous and append to non-contiguous
    
    temp_trend[, contiguous := NULL] # drop contiguous indicator
    
    setorder(temp_trend, cat1_group, cat2_group, Segment.Start)
    
    # create formatted trend summary
    temp_trend[, trend := paste0(Segment.Start, "-", substr(Segment.End, 3, 4), ": ", trend)]
    
    # drop start and end columns      
    temp_trend[, c("Segment.Start", "Segment.End") := NULL]
    
    # create a counter for each group.num
    temp_trend[,  count := 1:.N, by = .(cat1_group, cat2_group)]
    
    # reshape from long to wide
    temp_trend <- dcast(temp_trend, cat1_group + cat2_group ~ count, value.var = "trend")
    
    # append the trends across each row
    cols <- names(temp_trend)[3:ncol(temp_trend)]
    temp_trend[, time_trends:= apply(.SD, 1, function(x) paste(x, collapse = '; ')), .SDcols = cols]
    temp_trend[, time_trends := gsub("; NA", "", time_trends)]
    
    # keep columns of interest
    temp_trend <- temp_trend[, .(cat1_group, cat2_group, time_trends)]
    
    
    ### Join back to data and make trends tab for joining
    output <- left_join(output, temp_trend,
                        by = c("cat1_num" = "cat1_group",
                               "cat2_group_num" = "cat2_group")) %>%
      mutate(tab = "trends") %>%
      # Only need to keep one row per cat2_group since they're all the same
      distinct(tab, cat1_group, cat2_group, time_trends)
    
    
    
    ### Remove unwanted files made by JoinPoint
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

