# Guess the correct rps file in a slide directory
# If students want to work through slides at home, use the prefix "home_"
get.courser.slides.rps = function(slides.dir, home=FALSE, prefix= if (home) "home_" else "") {
  restore.point("get.courser.slides.rps")
  pres = paste0(prefix,rev(file.path.split(slides.dir))[1])
  files = list.files(slides.dir,pattern = glob2rx(paste0(prefix,"*.rps")))

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
presenterApp = function(courseid="", slides.dir, token.dir=NULL,clicker.dir=NULL, ps.file=get.courser.slides.rps(slides.dir), teacher="Teacher", query.key = NULL) {
  restore.point("presenterApp")

  if (is.null(ps.file)) {
    stop("Could not find an .rps file for your presentation.")
  }

  ps = read.rps(file.path(slides.dir,ps.file))

  # save.nothing = TRUE prevents creation of an .ups
  # file
  app = slidesApp(ps = ps,user.name = teacher,dir = slides.dir, opts=list(courseid=courseid, clicker.dir=clicker.dir, use.clicker=TRUE, save.nothing=TRUE))

  # require login by query.key (fixed or token) or cookie
  if (!is.null(query.key) | !is.null(token.dir)) {
    orgInitHandler = app$initHandler
    login.fun = function(...) {
      orgInitHandler(...,app=getApp())
    }
    login.failed.fun = function(...) {
      args = list(...)
      restore.point("failedPresenterAppLogin")
      evalJS('alert("No authorized user detected for slide presentation. Interactive functions are turned off.");')

      cat("\nlogin failed show without events...")
      #stopApp()
    }
    restore.point("presenterAppWithQueryKey")
    lop = loginModule(login.by.query.key = "require",fixed.query.key = query.key, login.fun = login.fun, login.failed.fun=login.failed.fun, token.dir=token.dir, cookie.name = "courserPresenterCookie")
    app$initHandler = function(...) {
      initLoginDispatch(lop)
    }
  }


  app
}

makePresenterAppDir = function(courseid,slides,teacher="Teacher", opts, hash=random.string(1,127), del.old.app.dirs = FALSE, query.key = NULL) {
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
    token.dir = file.path(opts$course.dir, "course", "teacher_tokens")

  # On a docker server, we have a fixed directory structure
  } else {
    slides.dir = file.path("/srv/slides",slides)
    clicker.dir = "/srv/clicker"
    token.dir = "/srv/tokens"
  }

  code = paste0('
# Automatically generated presentation app

library("courser")

# Local Mathjax does not correctly displays
options(use.local.mathjax=FALSE)

# turn off restore points for better performance
set.storing(FALSE)

slides.dir = "',slides.dir,'"
clicker.dir = "',opts$clicker.dir,'"
courseid = "',courseid,'"
token.dir = ',as.code.string(token.dir),'
query.key = ',as.code.string(query.key),'

app = presenterApp(courseid=courseid, slides.dir=slides.dir, clicker.dir=clicker.dir, query.key=query.key,token.dir=token.dir)

appReadyToRun(app)

shinyApp(ui = app$ui, server = app$server)
')
  app.file = file.path(app.dir, "app.R")
  try(writeLines(code, app.file))

  app.dir
}


as.code.string = function(x) {
  if (is.null(x)) return('NULL')
  if (is.na(x)) return('NA')
  if (is.character(x)) return(paste0('"',x,'"'))
  as.character(x)
}
