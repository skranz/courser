examples.coursepage = function() {
  restore.point.options(display.restore.point = TRUE)
  course.dir = "D:/libraries/courser/courses/vwl"
  app = CoursePageApp(course.dir=course.dir,init.userid="kranz", need.password=FALSE, need.user=TRUE, fixed.password="ompo", use.signup=FALSE)

  res = viewApp(app, port=app$glob$opts$student$port,launch.browser = rstudioapi::viewer)
  try(dbDisconnect(app$glob$studentdb))

}

student.schema = function(app=getApp()) {
  if (!is.null(app$glob[["studschema"]]))
    return(app$glob[["studschema"]])

  schema.file = system.file("schema/studentdb.yaml", package="courser")
  app$glob[["studschema"]] = rmdtools::read.yaml(schema.file)

}

get.studentdb = function(course.dir = cp$course.dir, db=app$glob[["studentdb"]], app = getApp(), cp=app$cp) {
  if (!is.null(db)) return(db)

  db.dir = file.path(course.dir, "db")
  db.file = file.path(db.dir,"students.sqlite")
  if (!file.exists(db.file)) {
    db = create.studentdb(course.dir=course.dir)
  } else {
    db = dbConnect(SQLite(),dbname = file.path(db.dir,"students.sqlite"))
  }
  if (!is.null(app$glob))
    app$glob$studentdb = db
  db
}

create.studentdb = function(course.dir, schema.file = NULL) {
  restore.point("create.studentdb")

  db.dir = file.path(course.dir, "db")
  if (!dir.exists(db.dir))
    dir.create(db.dir)

  db = dbConnect(SQLite(),dbname = file.path(db.dir,"students.sqlite"))
  if (is.null(schema.file))
    schema.file = system.file("schema/studentdb.yaml", package="courser")
  dbmisc::dbCreateSchemaTables(db, schema.file = schema.file)
  #dbDisconnect(db)
  db
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


coursepage.login = function(userid=app$cp$userid,app=getApp(),...) {
  restore.point("coursepage.login")

  cp = app$cp
  cp$userid = cp$email = userid


  db = get.studentdb()
  cp$stud = dbGet(db,"students",params = nlist(userid))
  # student does not yet exist
  # show modal settings window
  if (NROW(cp$stud) == 0) {
    settings.ui = student.settings.ui(cp=cp, submitBtn = actionButton("settingsModalBtn","Save and continue"))
    add.form.handlers(form=cp$settings.form,btn.id="settingsModalBtn",function(values,...)  {
      args = list(...)
      restore.point("settingsModalBtn")
      res = dbGet(db, "students", params=values["nick"])
      if (NROW(res)>0) {
        show.form.alert(form=form, msg=paste0("There is already a user with alias ", values$nick, ". Please pick another alias"))
      }
      schema = student.schema()
      values$email = values$userid
      values = values[names(schema$students$table)]

      dbInsert(db, "students", values, schema=schema)
      cp$stud = values
      removeModal()
      coursepage.login()
    })

    showModal(modalDialog(
      settings.ui,
      title = "Please enter your data",
      easyClose = FALSE,footer = NULL
    ))
    return()
  }
  cp$stud = as.list(cp$stud)


  cp$cp.ui = rmdtools::render.compiled.rmd(cp$cr, envir=cp,out.type = "shiny")
  cp$settings.ui = student.settings.ui(cp=cp,values = cp$stud)

  ui = tabsetPanel(
    tabPanel(cp$courseid,cp$cp.ui),
    tabPanel("Settings",cp$settings.ui)
  )

  setUI("mainUI",ui)
  dsetUI("mainUI",ui)



}

student.settings.ui = function(cp=app$cp, values = list(userid=cp$userid), submitBtn=NULL) {
  restore.point("student.settings.ui")
  lang = first.non.null(cp[["lang"]],"en")
  file = system.file(paste0("forms/",lang,"/student_settings.yaml"), package = "courser")
  cp$settings.form = yaml.form(file=file, lang=lang, prefix="stud_settings")
  ui = form.ui.simple(cp$settings.form, submitBtn=submitBtn,values = values)
  ui
}



compile.coursepage = function(course.dir, page.file = file.path(course.dir,"course_page.Rmd")) {
  cr = rmdtools::compile.rmd(file = page.file)
  cr
}
