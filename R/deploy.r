# Functions for deploying a course

course.settings = function(course.dir) {
  file = file.path(course.dir,"settings","settings.yaml")
  settings = read.yaml(file = file)
  settings
}

make.course.clicker.app = function(course.dir) {
  settings = course.settings(course.dir)
  app.dir = file.path(course.dir, "course","shiny-server","clicker")

  # server.R
  file = file.path(app.dir,"server.R")
  code = "shinyServer(app$server)"
  writeLines(code,file)

  # ui.R
  file = file.path(app.dir,"ui.R")
  code = "shinyUI(app$ui)"
  writeLines(code,file)


}


# TO BE DONE..
make.course.clicker.dirs = function(courseid=basename(course.dir), course.dir=getwd()) {
  clicker.dir = file.path(course.dir,"course", "clicker")
  make.dir(clicker.dir)

}


examples.make.course.skeleton = function() {

}

# TO DO: Best copy a template
make.course.skeleton = function(courseid, parent.dir=getwd()) {
  course.dir = file.path(parent.dir, courseid)
  make.dir(course.dir)

}

make.dir = function(dir,..., recursive=TRUE) {
  if (dir.exists(dir)) return()
  dir.create(dir,..., recursive=recursive)
}
