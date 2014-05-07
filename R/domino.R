# Environment(package global) variable holding infomation about domino client
# If this gets assigned with true, the commands will be tried to run from default installation path
domino <- new.env()
domino$command_is_in_the_path <- TRUE

# Runs a domino client command
# calls successCallback if command execution succedded
# prints failure message if command execution failed
# calls domino.runCommandFromDefaultPath if command is not found
domino.runCommand <- function(commandAndArgs, successCallback=domino.OK, failureMessage="Executing the command failed", stdInput=FALSE) {
  if(!domino$command_is_in_the_path) {
    # Call domino client directly from default path if we know that it's not in the PATH
    return(domino.runCommandFromDefaultPath(commandAndArgs, successCallback, failureMessage, stdInput))
  }
  cmd = paste("domino --source R", commandAndArgs)
  result = domino.call(cmd, stdInput)
  if (result == 0) {
    return(successCallback())
  }
  else if (result == 127) {
    if(domino$command_is_in_the_path){
      domino$command_is_in_the_path <- FALSE
      print("Domino client not found in system's PATH. Trying default location...")
    }
    return(domino.runCommandFromDefaultPath(commandAndArgs, successCallback, failureMessage, stdInput))
  }
  else {
    stop(failureMessage, call.=FALSE)
  }
}

# Runs Domino client command from default installer path
# calls successCallback if command execution succedded
# prints failure message if command execution failed
# prints domino.handleCommandNotFound message when there was no domino client found in default location.
domino.runCommandFromDefaultPath <- function(commandAndArgs, successCallback=domino.OK, failureMessage="Executing the command failed", stdInput=FALSE) {
  # handling of command not found
  prefix = domino.osSpecificPrefixOfDominoCommand()
  # join without spaces, as prefix and domino must stick together and commandAndArgs is already a string with spaces
  fixedCmd = paste(prefix, "domino --source R ", commandAndArgs, sep="")
  fixedResult = domino.call(fixedCmd, stdInput)
  if (fixedResult == 0) {
    return(successCallback())
  }
  else if (fixedResult == 127) {
    domino.handleCommandNotFound(failureMessage)
  }
  else {
    stop(failureMessage, call.=FALSE)
  }
}

#Calls a program with or without stdin
domino.call <- function(cmd, stdInput=FALSE) {
  if(domino.notFalse(stdInput)){
    return(system(cmd, input=stdInput))
  } else {
    return(system(cmd))
  }
}

domino.handleCommandNotFound = function(failureMessage){
  stop(paste("Couldn't find domino client in the PATH or in default locations.
  Add domino client directory path to PATH environment variable.
  If you don't have domino client installed follow instructions on 'http://help.dominoup.com/client'.
  If you use R-Studio Domino on GNU/Linux through a desktop launcher, add domino path to the .desktop file.

  If you need more help, email support@dominoup.zendesk.com or visit http://help.dominoup.com/troubleshooting

  - ", failureMessage), call.=FALSE)
}

domino.osSpecificPrefixOfDominoCommand <- function() {
  os = Sys.info()["sysname"]
  if(os == "Darwin") {return("/Applications/domino/")}
  else if (os =="Linux") {return("~/domino/")}
  else if (os == "Windows") {return("c:\\program files (x86)\\domino\\")}
  else { print("Your operating system is not supported by domino R package.")}
}

domino.notFalse <- function(arg) {
  if (arg == FALSE){FALSE} else { TRUE}
}

domino.OK <- function(){return(0)}

# Creates Domino project in a new folder under current working directory.
# project.name - is the name of the project to be fetched.
domino.login <- function(usernameOrEmail, password) {
  if(missing(usernameOrEmail) || missing(password)) {
    stop("Missing parameters for login command. Proper usage:domino.login(usernameOrEmail, password)")
  }
  theinput = paste(usernameOrEmail, '\n', password, sep="")
  domino.runCommand("login", domino.OK, "Login failed", theinput)
}

domino.projectNameWithoutUser <- function(projectName) {
  rev(unlist(strsplit(projectName, "/")))[1]
}

domino.jumpToProjectsWorkingDirectory <- function(projectName) {
  setwd(paste("./",domino.projectNameWithoutUser(projectName), sep=""))
  print("Changed working directory to new project's directory.")
}

# Creates Domino project in a new folder in current working directory.
# project.name - is the name of the project to be fetched.
domino.create <- function(projectName) {
  if(missing(projectName)) {
    stop("Missing parameters for create command. Proper usage:domino.create(projectName)", call.=FALSE)
  }
  cmd = paste("create", projectName)
  goToProjectCallback = function(){
    domino.jumpToProjectsWorkingDirectory(projectName)
  }
  domino.runCommand(cmd, goToProjectCallback, "Creating project failed")
}

# Downloads existing Domino project from the server
# project.name - is the name of the project to be fetched. It can take form of username/projectname or just projectname
domino.get <- function(projectName) {
  if(missing(projectName)) {
    stop("Missing parameters for get command. Proper usage:domino.get(projectName)", call.=FALSE)
  }
  cmd = paste("get", projectName)
  goToProjectCallback = function(){
    domino.jumpToProjectsWorkingDirectory(projectName)
  }
  domino.runCommand(cmd, goToProjectCallback, "Downloading project data failed")
}

# Starts domino project in current working directory
# projectName - lets you define project name different than current directory name
domino.init <- function(projectName) {
  if(missing(projectName)) {
    stop("Missing parameters for init command. Proper usage:domino.init(projectName)", call.=FALSE)
  }
  cmd = paste("init", projectName)
  domino.runCommand(cmd, domino.OK, "Initializing the project failed")
}

domino.debug <- function() {
  domino.runCommand("debug")
}

domino.diff <- function() {
  domino.runCommand("diff")
}

domino.download <- function() {
  domino.runCommand("download", domino.OK, "Downloading project data failed.")
}

domino.upload <- function() {
  domino.runCommand("upload", domino.OK, "Uploading project data failed.")
}

domino.dump <- function() {
  domino.runCommand("dump")
}

domino.reset <- function() {
  domino.runCommand("reset")
}

domino.run <- function(...) {
  if(missing(...)) {
    stop("Missing parameters for run command. Example usage:domino.run('main.R', param1, param2, param3, ...)", call.=FALSE)
  }
  cmd = paste("run", ...)
  domino.runCommand(cmd, domino.OK, paste("Running the \"", cmd,"\" command failed", sep=""))
}

domino.status <- function(...) {
  cmd = paste("status", ...)
  domino.runCommand(cmd, domino.OK)
}
