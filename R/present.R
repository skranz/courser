get.presenter.ps.file = function(slides.dir) {
  restore.point("get.presenter.ps.file")
  pres = rev(file.path.split(slides.dir))[1]

  files = list.files(slides.dir,pattern = glob2rx("*.rps"))
  if (length(files)==0) return(NULL)
  if (length(files)==1) return(files)

  base = tools::file_path_sans_ext(files)
  ind = match(pres,base)
  if (!is.na(ind)) return(files[ind])

  ind = match(str.left.of(pres,"-"),base)
  if (!is.na(ind)) return(files[ind])

  return(files[1])
}

# We have a single presenter app for each course
presenterApp = function(courseid="", slides.dir, token.dir,clicker.dir=NULL, ps.file=get.presenter.ps.file(slides.dir), teacher="Teacher", query.key = NULL) {
  restore.point("presenterApp")

  if (is.null(ps.file)) {
    stop("Could not find an .rps file for your presentation.")
  }

  ps = read.rps(file.path(slides.dir,ps.file))

  app = slidesApp(ps = ps,user.name = teacher,dir = slides.dir, opts=list(courseid=courseid, clicker.dir=clicker.dir, use.clicker=TRUE))

  # require correct query key in url
  if (!is.null(query.key)) {
    login.fun = app$initHandler
    login.failed.fun = function(...) {
      args = list(...)
      restore.point("failedPresenterAppLogin")
      cat("\nlogin failed show without events...")
      #stopApp()
    }
    restore.point("presenterAppWithQueryKey")
    lop = loginModule(login.by.query.key = "require",fixed.query.key = query.key, login.fun = login.fun, login.failed.fun=login.failed.fun )
    app$initHandler = function(...) {
      initLoginDispatch(lop)
    }
  }


  app
}

makePresenterAppDir = function(courseid,slides,teacher="Teacher", opts, hash=random.string(1,127),token.dir = "", del.old.app.dirs = FALSE, query.key = NULL) {
  restore.point("makePresenterAppDir")
  #stop()

  course.dir = opts$course.dir
  app.base.dir = file.path(course.dir,"course","shiny-server","present","slides")
  app.dir = file.path(app.base.dir, slides)

  if (del.old.app.dirs) {
    dirs = setdiff(list.dirs(app.base.dir, recursive=FALSE),app.dir)
    for (dir in dirs) {
      try(unlink(dir))
    }
  }

  if (!dir.exists(app.dir)) {
    #dir.create(app.dir,recursive = TRUE, mode="744")
    dir.create(app.dir,recursive = TRUE)
    Sys.chmod(app.dir, mode="777", use_umask = FALSE)

  }
  figure.dir = file.path(app.dir,"figure")
  if (!dir.exists(figure.dir)) {
    dir.create(figure.dir,recursive = TRUE)
    Sys.chmod(figure.dir, mode="777", use_umask = FALSE)
  }

  if (opts$local) {
    slides.dir = file.path(opts$course.dir,"slides",slides)
    clicker.dir = file.path(opts$course.dir,"course","clicker")

  # On a docker server, we have a fixed directory structure
  } else {
    slides.dir = file.path("/srv/slides",slides)
    clicker.dir = "/srv/clicker"
  }

  # token.dir not yet used
  token.dir = NA
  if (!is.null(query.key)) {
    query.key.str = paste0('query.key = "', query.key,'"')
  } else {
    query.key.str = paste0('query.key = NULL')
  }

  code = paste0('
# Automatically generated presentation app

library("courser")
slides.dir = "',slides.dir,'"
clicker.dir = "',opts$clicker.dir,'"
token.dir = "',token.dir,'"
courseid = "',courseid,'"
',query.key.str,'

app = presenterApp(courseid=courseid, slides.dir=slides.dir, token.dir=token.dir, clicker.dir=clicker.dir, query.key=query.key)

appReadyToRun(app)

shinyApp(ui = app$ui, server = app$server)
')
  app.file = file.path(app.dir, "app.R")
  try(writeLines(code, app.file))

  app.dir
}
