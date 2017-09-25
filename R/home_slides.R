examples.courserHomeSlidesApp = function() {
  restore.point.options(display.restore.point = TRUE)

  course.dir = "D:/libraries/courser/courses/vwl"
  courseid   = "vwl"
  slides     = "Kap1"
  token.dir  = file.path(course.dir,"course","tokens")
  slides.dir = file.path(course.dir,"slides", slides)
  clicker.dir= file.path(course.dir,"course","clicker")

  app = courserHomeSlidesApp(courseid=courseid,slides.dir=slides.dir,token.dir=token.dir, clicker.dir=clicker.dir, app.title=slides, require.query.key = TRUE)

  queryBrowser = function(url = NULL,..., args=NULL) {
    if (!is.null(args)) {
      url = paste0(url, "?", paste0(names(args),"=", args, collapse="&"))
    }
    browseURL(url, ...)
  }

  res = viewApp(app, port=app$glob$opts$student$port,launch.browser = rstudioapi::viewer)

}


# We have an App to watch a slide at home
# course.dir is not directly used
courserHomeSlidesApp = function(courseid="", slides.dir, token.dir,clicker.dir, ps.file=get.courser.slides.rps(slides.dir,home=TRUE), course.dir=NULL, require.query.key = TRUE, app.title="Slides") {
  restore.point("courserHomeSlidesApp")

  if (is.null(ps.file)) {
    stop(paste0("Could not find a home .rps file in slides.dir ", slides.dir,"."))
  }

  ps = read.rps(file.path(slides.dir,ps.file))

  app = slidesApp(ps = ps,user.name = ".HOME.",dir = slides.dir, opts=list(courseid=courseid, clicker.dir=clicker.dir, rtutor=TRUE))

  # require correct query key in url
  if (require.query.key) {
    login.fun = app$initHandler
    login.failed.fun = function(...) {
      args = list(...)
      restore.point("failedcourserHomeSlidesAppLogin")
      cat("\nlogin failed, don't show slides...")
      setInnerHTML(selector="body", html = "Unknown user key. No slides are shown.")
      #stopApp()
    }
    restore.point("presenterAppWithQueryKey")
    lop = loginModule(login.by.query.key = "require", login.fun = login.fun, login.failed.fun=login.failed.fun,token.dir=token.dir )
    app$initHandler = function(...) {
      restore.point("homeSlidesQueryLogin")
      initLoginDispatch(lop)
    }
  }
  app
}

makeHomeSlidesAppDir = function(courseid,slides, opts, hash=random.string(1,127),token.dir = "", del.old.app.dirs = FALSE, query.key = NULL) {
  restore.point("makeHomeSlidesAppDir")
  #stop()

  course.dir = opts$course.dir
  app.base.dir = file.path(course.dir,"course","shiny-server","home","slides")
  app.dir = file.path(app.base.dir, slides)
  token.dir=file.path(course.dir,"course","tokens")

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

  } else {
    # In a docker container, we have a fixed directory structure
    slides.dir = file.path("/srv/slides",slides)
    clicker.dir = "/srv/clicker"
  }


  code = paste0('
# Automatically generated presentation app

library("courser")
slides.dir = "',slides.dir,'"
clicker.dir = "',opts$clicker.dir,'"
token.dir = "',token.dir,'"
courseid = "',courseid,'"

app = courserHomeSlidesApp(courseid=courseid, slides.dir=slides.dir, token.dir=token.dir, clicker.dir=clicker.dir)

appReadyToRun(app)

shinyApp(ui = app$ui, server = app$server)
')
  app.file = file.path(app.dir, "app.R")
  try(writeLines(code, app.file))

  app.dir
}
