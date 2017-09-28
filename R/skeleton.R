# Create an empty course dir with some default content
courser.skeleton=function(course.dir) {
  # TO DO: Create an example directory in courser/inst
  # that will be copied to course.dir

  #dir.create.nonexisting(course.dir)
  #dir.create.nonexisting(file.path(course.dir,"slides"))
  #dir.create.nonexisting(file.path(course.dir,"course"))
  #dir.create.nonexisting(file.path(course.dir,"course","stud_tokens"))
  #dir.create.nonexisting(file.path(course.dir,"course","teacher_tokens"))
  #dir.create.nonexisting(file.path(course.dir,"course","shiny-server"))

  #dir.create.nonexisting(file.path(course.dir,"course","settings"))


}

dir.create.nonexisting = function(dir, recursive=TRUE,...) {
  if (!dir.exists(dir)) dir.create(dir, recursive = recursive,...)
}
