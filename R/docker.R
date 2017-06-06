examples.run.docker.container = function() {

  tgroup.dir = "D:/libraries/RTutorTeacher/teacherhub/tgroups/kranz"
  opts = yaml.load_file(file.path(tgroup.dir,"settings","settings.yaml"))
  tgroup.dir = "/home/sk/docker/RTutorTeacher/tgroups/kranz"

  th.make.docker.script(tgroup.dir, opts=opts)

  #th.run.docker.container(tgroup.dir)
}

th.make.docker.script = function(tgroup.dir, opts = yaml.load_file(file.path(tgroup.dir,"settings","settings.yaml")), image="skranz/rtutorteacher_man", use.rstudio = TRUE) {
  restore.point("run.docker.container")



  opts = make.th.container.settings(opts)


  shiny.dir = file.path(tgroup.dir,"shiny-server")
  teachers.dir = file.path(tgroup.dir,"teachers")
  clicker.dir = file.path(tgroup.dir,"clicker")
  present.dir = file.path(shiny.dir,"present")
  log.dir = file.path(tgroup.dir,"log")


  code = paste0(
"# Run TeacherHub Docker containers
docker pull ", image,"

# For rstudio support adapt and add to docker run
-p 8711:8787 -e ROOT=TRUE -e USER=admin -e PASSWORD=<yourpassword> -e RUN_RSTUDIO=yes
")


  rstudio = if (use.rstudio) {
    paste0("-p 8711:8787 -e ROOT=TRUE -e USER=admin -e PASSWORD=<yourpassword> -e RUN_RSTUDIO=yes")
  } else {
    "-e RUN_RSTUDIO=no"
  }

  # teacherhub
  field = "teacherhub"
  name = opts[[field]]$container
  port = opts[[field]]$port

  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 ',rstudio,' -v ',file.path(shiny.dir,field),':/srv/shiny-server/',field,' -v ',tgroup.dir,':/srv/tgroup -v ',teachers.dir,':/srv/teachers -v ', log.dir,':/var/log/ -v ', clicker.dir,':/srv/clicker/ ' ,image)

  code = c(code,"",paste0("# ", field), paste0("docker stop ", name),paste0("docker rm ", name), run.com)

  # present
  field = "present"
  name = opts[[field]]$container
  port = opts[[field]]$port

  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 -e ROOT=FALSE -e RUN_RSTUDIO=no -v ',file.path(shiny.dir,field),':/srv/shiny-server/',field,' -v ',teachers.dir,':/srv/teachers -v ', log.dir,':/var/log/ -v ', clicker.dir,':/srv/clicker/ ',image)

  code = c(code,"",paste0("# ", field), paste0("docker stop ", name),paste0("docker rm ", name), run.com)


  # clicker
  field = "clicker"
  name = opts[[field]]$container
  port = opts[[field]]$port

  run.com = paste0('docker run -entrypoint="/usr/bin/with-contenv bash" --name ',name,' -d -p ',port,':3838 -e ROOT=FALSE -e RUN_RSTUDIO=no -v ',file.path(shiny.dir,field),':/srv/shiny-server/',field,'  -v ', log.dir,':/var/log/ -v ', clicker.dir,':/srv/clicker/ ',image)

  code = c(code,"",paste0("# ", field), paste0("docker stop ", name),paste0("docker rm ", name), run.com)

  cat(paste0(code,collapse="\n"))
  writeClipboard(code)
  invisible(code)
}


th.run.docker.container = function(tgroup.dir, tag="latest") {
  restore.point("run.docker.container")

  opts = yaml.load_file(file.path(tgroup.dir,"settings","settings.yaml"))

  opts = make.th.container.settings(opts)


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

make.th.container.settings = function(opts) {
  fields = c("teacherhub","present","clicker")

  for (field in fields) {
    opts[[field]]$container = first.non.null(opts[[field]]$container,paste0(opts$container_prefix,"_",field))
  }
  opts
}
