
# Install Dependencies ----------------------------------------------------

#' Load biocLite Package, Installing if Necessary
#' 
#' If the package is already installed, it is laoded with \code{require}. If
#' it is not installed, it is installed using the \code{biocLite} function
#' sourced from \url{https://bioconductor.org/biocLite.R}.
#'
#' @param package Name (as a string) of package to install
#'
#' @return NULL
#' @export
#'
#' @examples
biocLiteInstall = function(package) {
  if(!require(package,character.only = TRUE)) {
    source("https://bioconductor.org/biocLite.R")
    biocLite(package)
  }
}

biocLiteInstall("GO.db")
biocLiteInstall("impute")
biocLiteInstall("preprocessCore")

require(WGCNA)
require(stringr)
require(survival)
require(yaml)


# Helper Functions --------------------------------------------------------

#' Get a String Response from User
#' 
#' If \code{default} is specified and non-null, if the user enters a blank
#' string as their response, \code{default} is used as the response instead.
#' The specified \code{default} is also displayed after the prompt like so:
#' \code{<prompt> [<default>]: }. If it is not specified, only \code{: } is
#' appended to the \code{prompt}
#'
#' @param prompt Prompt to display to the user
#' @param default Response to use if user enters blank string
#'
#' @return Response stripped of surrounding quotes if applicable
#' @export
#'
#' @examples
getString = function(prompt, default = NULL) {
  help = ": "
  if (!is.null(default)) {
    help = paste(" [", default, "]: ", sep = "")
  }
  prompt = paste(prompt, help, sep = "")
  response = readline(prompt)
  # Caret and dollar sign respectively match start and end of line
  # Matches and returns the middle part (.*) without any surrounding quotes
  response = gsub("^[\"'](.*)[\"']$", "\\1", response)
  if (!is.null(default) && response == "") {
    response = default
  }
  return(response);
}

#' Get a Yes or No Response from User
#' 
#' The user is prompted to answer a yes-or-no question and is re-prompted if
#' they enter invalid responses. If \code{default} is specified and non-null,
#' it is used in place of the user's response. Only the first letter of the
#' response is considered. Affirmative responses start with \code{Y} or
#' \code{y}, and negative ones start with \code{N} or \code{n}. Any other first
#' letter makes the response invalid. The provided prompt is suffixed by helper
#' text of the form \code{ [y/n] }, where the default option, if provided, is
#' capitalized (e.g. \code{ [Y/n] } for defaulting to affirmative).
#' @param prompt The yes-or-no question displayed to the user
#' @param default Default option should the user enter a blank response
#' @return \code{TRUE} for an affirmative response, \code{FALSE} for a negative
#'         one
#' @export
#'
#' @examples
getYesOrNo = function(prompt, default = NULL) {
  help = "[y/n]"
  if (!is.null(default)) {
    defaultLetter = tolower(substr(default, 1, 1))
    if (defaultLetter == "y") {
      help = "[Y/n]"
    } else if (defaultLetter == "n") {
      help = "[y/N]"
    }
  }
  prompt = paste(prompt, " ", help, ": ", sep = "")
  response = tolower(substr(readline(prompt), 1, 1));
  if (response == "") {
    response = defaultLetter
  }
  while (response != "y" && response != "n") {
    response = readline("Please enter either 'yes' or 'no': ")
    response = tolower(substr(response, 1, 1))
  }
  return(response == "y");
}

# SOURCE: behavior.R
# TODO: Re-write this
# Prompts the user with <prompt> to enter an option from <choices>. Entering "l"
# causes the choices to be listed. Uses .autocomplete() and removes quotes.
# TODO Roxygen
getInputFromList = function(prompt, choices, quit = NULL, list = "l", 
                            printFn = print, reprompt = "Invalid input.",
                             caseSensitive = FALSE, removeSpaces = FALSE) {
  menu = if (!is.null(list)) c(choices, list) else choices;
  menu = if (!is.null(quit)) c(menu, quit) else menu;
  reprompt = paste(reprompt, prompt, sep = '\n');
  
  userInput = readline(prompt);
  repeat {
    userInput = gsub('^["\']','', gsub('["\']$','', userInput));
    if (removeSpaces) userInput = gsub(' ', '', userInput);
    userInput = .autocomplete(userInput, menu, caseSensitive);
    if (userInput %in% choices) {
      break;
    } else if (!is.null(list) && userInput == list) {
      printFn(choices);
      userInput = readline(prompt);
    } else if (!is.null(quit) && userInput == quit) {
      return(quit);
    } else {
      userInput = readline(reprompt);
    }
  }
  return(userInput);
}

notify = function(level, message, details = NULL) {
  level = tolower(level)
  levels = c("silence", "error", "warning", "info", "debug", "verbose")
  thisLevelNum = match(level, levels)
  setLevelNum = match(verbosity, levels)
  
  if (is.na(thisLevelNum) || is.na(setLevelNum)) {
    stop(paste("Invalid warning levels for message=[", message, "], level=", 
               level, ", setLevel=", verbosity, sep = ""))
  }
  
  if (thisLevelNum <= setLevelNum) {
    if (thisLevelNum <= match("warning", levels)) {
      warning(message, immediate. = TRUE)
      if (!is.null(details) && getYesOrNo("Show details?", "No")) {
        cat(paste(details, "\n", sep = ""))
      }
      if (thisLevelNum <= match("warning", levels) && getYesOrNo("Abort?", 
                                                                 "Yes")) {
        stop("Script aborted by user in response to error or warning.")
      }
    } else {
      cat(paste(message, "\n", sep = ""))
      cat(paste(details, "\n", sep = ""))
    }
  }
}


find_match = function(targets, list, level, autoPick = TRUE) {
  matches = character()
  for (target in targets) {
    found = match_helper(target, list, level, autoPick)
    matches = c(matches, found)
  }
  return(matches)
}

match_helper = function(target, list, level, autoPick = TRUE) {
  found = grep(target, list, value = TRUE)
  if (length(found) > 1) {
    notify(level, paste("Multiple matches found for ", target, ".", sep = ""), 
           list)
    if (autoPick) {
      found = list[1]
    }
  } else if (length(found) < 1) {
    notify(level, paste("No match found for ", target, ".", sep = ""), list)
    if (autoPick) {
      found = ""
    }
  }
  return(found)
}

# Pairing function used to pair up behavior logs across timepoints
# TODO: Roxygen
pairer = function(subjectLog, followupGroup) {
  # Try and find a match using just the subject identifier
  subject = str_extract(subjectLog, subjRegex)
  pairLog = find_match(subject, followupGroup, "debug", autoPick = FALSE)
  if (length(pairLog) == 1) {
    return(pairLog)
  }
  # Try and find a match by changing date and time
  time_log = other_time(subjectLog)
  day_log  = other_day(subjectLog)
  time_day_log = other_time(other_day(subjectLog))
  potential_log_names = c(time_log, day_log, time_day_log)
  match = find_match(potential_log_names, followupGroup, "debug", 
                     autoPick = FALSE)
  if (length(match == 1)) {
    return (match)
  }
  # Give up automatically choosing and ask the user to pick
  print(followupGroup)
  pairLog = getInputFromList(paste("Select log to pair with ", subjectLog, ".", 
                                   sep = ""), followupGroup)
  notify("debug", paste("Paired log for ", subjectLog, "(subject = ", subject, 
                        ") found as ", pairLog, sep = ""))
  return(pairLog)
}

other_time = function(subjectLog) {
  if (grepl("Morning", subjectLog)) {
    otherLog = str_replace(subjectLog, "Morning", "Afternoon")
    otherLog = str_replace(otherLog, "Morn", "Aft")
  } else {
    otherLog = str_replace(subjectLog, "Afternoon", "Morning")
    otherLog = str_replace(otherLog, "Aft", "Morn")
  }
  notify("debug", paste("Generated other time log ", otherLog, " for log ", 
                        subjectLog, sep = ""))
  return(otherLog)
}

other_day = function(logName) {
  core = str_extract(logName, fileRegex)
  prefix = str_replace(logName, paste(core, ".txt", sep = ""), "")
  date = substr(core, 1, 6)
  form = "%m%d%y"
  date = as.Date(date, format = form)
  if (grepl("D2", prefix)) {
    adj = date - 1
    adjPrefix = str_replace(prefix, "D2/D2_", "D1/D1_")
  } else {
    adj = date + 1
    adjPrefix = str_replace(prefix, "D1/D1_", "D2/D2_")
  }
  adj = format(adj, form)
  adjCore = paste(adj, substr(core, 7, nchar(core)), sep = "")
  adjLog = str_replace(logName, core, adjCore)
  adjLog = str_replace(adjLog, prefix, adjPrefix)
  notify("debug", paste("Generated other day log ", adjLog, " for log ", 
                        logName, sep = ""))
  return(adjLog)
}

timepoint_group_namer = function(group, timepoint, folders) {
  splitTime = strsplit(timepoint, "_")[[1]]
  day = splitTime[1]
  time = splitTime[2]
  name = paste(day, "_", day, "_", group, "_", time, sep = "")
  folder = find_match(name, folders, "warning")
  return(folder)
}

# Setup Configuration -----------------------------------------------------

useConf = getYesOrNo("Use a configuration file?", "yes")
if (useConf) {
  confPath = getString("Path to configuration file: ", "conf.yml")
  config = yaml.load_file(confPath)
  
  behaviorcodeRoot = config$behaviorcode_path
  dataPath = config$data_path
  stitchLogs = config$stitch_logs
  fileRegex = config$fileRegex
  subjRegex = config$subjRegex
  verbosity = config$verbosity
  combineAllBehaviors = config$combine_all_behaviors
  behavCombinOrders = config$combine_behaviors
} else {
  behaviorcodeRoot = getString("Path to root of behaviorcode repository")
  dataPath = getString("Path to properly structured data folder")
  stitchLogs = getYesOrNo("Do you need to stitch logs together?", "no")
  verbosity = "info"
  combineAllBehaviors = FALSE
  behavCombinOrders = character()
}


# Load and Process Data ---------------------------------------------------

# Source non-package dependencies from behaviorcode
toSource = c("behavior.R", "bootstrap_rewrite2.R", "clusterycode.R", 
             "powerBootstrap2Independent_fast.R")
for (suffix in toSource) {
  path = paste(behaviorcodeRoot, suffix, sep="")
  source(path)
}

# Load data
data <- .getDataBatch(dataPath)

# Stitch logs together
if (stitchLogs) {
  data <- .stitchLogsTogether(data)
}

# Pair up logs across timepoints using pairer function
# Folders for timepoints and groups are named by timepoint_group_namer
data <- .pairGroups(data, pairer, timepoint_group_namer)

# Ignore behaviors specified in configuration file if present
if (exists("config")) {
  data <- .filterDataList(data, toExclude = config$ignore_behaviors)
}

# Combine all behaviors if so configured
if (combineAllBehaviors) {
  behaviors = names(.findDupBehaviors(data))
  notify("debug", "Combined all behaviors into 'All Behaviors'")
  data <- .replaceBehAll(data, behaviors, "All Behaviors")
} else {
  # Combine behaviors according to configuration file if present
  for (order in behavCombinOrders) {
    combined = order$combined
    toCombine = order$toCombine
    notify("debug", paste("Combining the following behaviors into ", combined, 
                          ": ", toCombine, sep = ""))
    data <- .replaceBehAll(data, toCombine, combined)
  }
}

# TODO: Make behaviors durational if needed

# Create Folder for Output
timestamp = format(Sys.time(), "%m%d%y-%H%M%S")
folder = paste("Output", timestamp, "/", sep = "")
dir.create(folder)

# Backup data
dump("data", file = paste(folder, "backup.R", sep = ""))


# Analyze Data ------------------------------------------------------------

# Perform basic statistical analysis
.compareBasicStats(data, folder)