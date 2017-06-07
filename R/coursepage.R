examples.coursepage = function() {
  restore.point.options(display.restore.point = TRUE)
  course.dir = "D:/libraries/courser/courses/vwl"
  app = CoursePageApp(course.dir=course.dir,init.userid="kranz", need.password=FALSE, need.user=TRUE, fixed.password="ompo", use.signup=FALSE)

  res = viewApp(app, port=app$glob$opts$student$port,launch.browser = rstudioapi::viewer)


}


CoursePageApp = function(course.dir, courseid = basename(course.dir)
, login.db.dir=NULL, app.title=paste0(courseid), ...) {
  restore.point("CoursePageApp")
  app = eventsApp()

  app$ui = fluidPage(
    uiOutput("mainUI")
  )

  opts = init.th.opts(course.dir = course.dir)
  opts$courseid = courseid
  cp = as.environment(opts)
  app$cp = cp
  cp$cr = compile.coursepage(course.dir=course.dir)

  db.arg = list(dbname=paste0(login.db.dir,"/userDB.sqlite"),drv=SQLite())
  lop = loginModule(db.arg = db.arg, login.fun=coursepage.login, app.title=app.title,container.id = "mainUI",...)

  restore.point("CoursePageApp.with.lop")

  appInitHandler(function(...,app=getApp()) {
    initLoginDispatch(lop)
  })
  app
}


coursepage.login = function(userid,app=getApp(),...) {
  restore.point("coursepage.login")

  cp = app$cp
  cp$userid = userid

  cp$cp.ui = rmdtools::render.compiled.rmd(cp$cr, envir=cp,out.type = "shiny")

  cp$settings.ui = student.settings.ui(cp=cp)

  ui = tabsetPanel(
    tabPanel(cp$courseid,cp$cp.ui),
    tabPanel("Settings",cp$settings.ui)
  )

  setUI("mainUI",ui)
  dsetUI("mainUI",ui)
}

student.settings.ui = function(cp=app$cp) {
  restore.point("student.settings.ui")
  lang = first.non.null(cp[["lang"]],"en")
  file = system.file(paste0("forms/",lang,"/student_settings.yaml"), package = "courser")
  form = yaml.form(file=file, lang=lang, prefix="stud_settings")
  ui = form.ui.simple(form)
  ui
}



compile.coursepage = function(course.dir, page.file = file.path(course.dir,"course_page.Rmd")) {
  cr = rmdtools::compile.rmd(file = page.file)
  cr
}
