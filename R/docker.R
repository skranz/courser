examples.run.docker.container = function() {
  source.dir ="D:/libraries/courser/courses/vwl"
  opts = read.yaml(file=file.path(source.dir,"course", "settings","settings.yaml"))
  course.dir = "D:/libraries/courser/courses/vwl"

  course.dir = "/home/skranz/docker/courses/vwl"
  courser.make.docker.script(course.dir, opts=opts)

  #th.run.docker.container(tgroup.dir)
}

courser.make.docker.script = function(course.dir, opts = read.yaml(file=file.path(course.dir,"course", "settings","settings.yaml")), image="skranz/courser", use.rstudio = TRUE, rstudio.port.add=5600) {
  restore.point("courser.make.docker.script")

  opts = make.courser.container.settings(opts)

  shiny.dir = file.path(course.dir,"course", "shiny-server")


  clicker.dir = file.path(course.dir,"course", "clicker")
  slides.dir = file.path(course.dir,"slides")

  present.dir = file.path(shiny.dir,"present")
  log.dir = file.path(course.dir,"course", "log")


  code = paste0(
"# Run TeacherHub Docker containers
docker pull ", image,"

# For rstudio support adapt and add to docker run
-p 8711:8787 -e ROOT=TRUE -e USER=admin -e PASSWORD=<yourpassword> -e RUN_RSTUDIO=yes
")



  # teacherhub
  field = "teacherhub"
  name = opts[[field]]$container
  port = opts[[field]]$port
  rstudio = if (use.rstudio) {
    paste0("-p ",port+rstudio.port.add,":8787 -e ROOT=TRUE -e USER=admin -e PASSWORD=<yourpassword>")
  } else {""}

  # link course/shiny-server/teacherhub directly to /srv/shiny-server
  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 ',rstudio,' -v ',file.path(shiny.dir,field),':/srv/shiny-server/ -v ',course.dir,':/srv/course ' ,image)

  code = c(code,"",paste0("# ", field), paste0("docker stop ", name),paste0("docker rm ", name), run.com)

  # present
  field = "present"
  name = opts[[field]]$container
  port = opts[[field]]$port
  rstudio = if (use.rstudio) {
    paste0("-p ",port+rstudio.port.add,":8787 -e ROOT=TRUE -e USER=admin -e PASSWORD=<yourpassword>")
  } else {""}

  # link course/shiny-server/present directly to /srv/shiny-server
  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 ',rstudio,' -v ',file.path(shiny.dir,field),':/srv/shiny-server/ -v ', slides.dir,':/srv/slides/ -v ', log.dir,':/var/log/ -v ', clicker.dir,':/srv/clicker/ ',image)

  code = c(code,"",paste0("# ", field), paste0("docker stop ", name),paste0("docker rm ", name), run.com)


  # clicker
  field = "clicker"
  name = opts[[field]]$container
  port = opts[[field]]$port
  rstudio = if (use.rstudio) {
    paste0("-p ",port+rstudio.port.add,":8787 -e ROOT=TRUE -e USER=admin -e PASSWORD=<yourpassword>")
  } else {""}

  # link course/shiny-server/clicker directly to /srv/shiny-server
  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 ',rstudio,' -v ',file.path(shiny.dir,field),':/srv/shiny-server/  -v ', log.dir,':/var/log/ -v ', clicker.dir,':/srv/clicker/ ',image)

  code = c(code,"",paste0("# ", field), paste0("docker stop ", name),paste0("docker rm ", name), run.com)

  cat(paste0(code,collapse="\n"))
  writeClipboard(code)
  invisible(code)
}


courser.run.docker.container = function(tgroup.dir, tag="latest") {
  restore.point("run.docker.container")

  opts = yaml.load_file(file.path(tgroup.dir,"settings","settings.yaml"))

  opts = make.courser.container.settings(opts)


  shiny.dir = file.path(tgroup.dir,"shiny-server")
  teachers.dir = file.path(tgroup.dir,"teachers")
  clicker.dir = file.path(tgroup.dir,"clicker")
  log.dir = file.path(tgroup.dir,"log")



  # teacherhub
  field = "teacherhub"
  name = opts[[field]]$container
  port = opts[[field]]$port

  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 -e ROOT=FALSE -e RUN_RSTUDIO=no -v ',file.path(shiny.dir,field),':/srv/shiny-server/',field,' -v ',tgroup.dir,':/srv/tgroup -v ', log.dir,':/var/log/ skranz/RTutorTeacher:',tag)

  rerun.container(name, run.com)

  # present
  field = "present"
  name = opts[[field]]$container
  port = opts[[field]]$port

  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 -e ROOT=FALSE -e RUN_RSTUDIO=no -v ',file.path(shiny.dir,field),':/srv/shiny-server/',field,' -v ',teachers.dir,':/srv/teachers -v ', log.dir,':/var/log/ -v ', clicker.dir,':/srv/clicker/ skranz/RTutorTeacher:',tag)

  rerun.container(name, run.com)


  # clicker
  field = "clicker"
  name = opts[[field]]$container
  port = opts[[field]]$port

  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 -e ROOT=FALSE -e RUN_RSTUDIO=no -v ',file.path(shiny.dir,field),':/srv/shiny-server/',field,'  -v ', log.dir,':/var/log/ -v ', clicker.dir,':/srv/clicker/ skranz/RTutorTeacher:',tag)

  rerun.container(name, run.com)

}

rerun.container = function(name, run.com) {
  system(paste0("docker stop ", name))
  system(paste0("docker rm ", name))
  system(run.com)
}

restart.container = function(name) {
  system(paste0("docker stop ", name))
  system(paste0("docker start ", name))
}

make.courser.container.settings = function(opts) {
  fields = c("teacherhub","present","clicker")

  for (field in fields) {
    opts[[field]]$container = first.non.null(opts[[field]]$container,paste0(opts$container_prefix,"_",field))
  }
  opts
}
