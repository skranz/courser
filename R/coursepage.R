examples.coursepage = function() {
  restore.point.options(display.restore.point = TRUE)
  course.dir = "D:/libraries/courser/courses/vwl"
  app = CoursePageApp(course.dir=course.dir,init.userid="random_10", need.password=FALSE, need.user=TRUE, fixed.password="test", use.signup=FALSE, send.welcome.email = FALSE)

  res = viewApp(app, port=app$glob$opts$student$port,launch.browser = rstudioapi::viewer)
  try(dbDisconnect(app$glob$studentdb))

}

student.schemas = function(app=getApp()) {
  restore.point("student.schema")


  if (!is.null(app$glob[["studschemas"]]))
    return(app$glob[["studschemas"]])

  schema.file = system.file("schema/studentdb.yaml", package="courser")
  app$glob[["studschemas"]] = load.and.init.schemas(schema.file)

}

get.studentdb = function(course.dir = cp$course.dir, db=app$glob[["studentdb"]], app = getApp(), cp=app$cp) {
  if (!is.null(db)) return(db)

  db.dir = file.path(course.dir,"course", "db")
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

  db.dir = file.path(course.dir,"course", "db")
  if (!dir.exists(db.dir))
    dir.create(db.dir)

  db = dbConnect(SQLite(),dbname = file.path(db.dir,"students.sqlite"))
  if (is.null(schema.file))
    schema.file = system.file("schema/studentdb.yaml", package="courser")
  dbmisc::dbCreateSchemaTables(db, schema.file = schema.file)
  #dbDisconnect(db)
  db
}

CoursePageApp = function(course.dir, courseid = basename(course.dir), login.db.dir=NULL, app.title=paste0(courseid), login.by.query.key="allow",  token.dir = file.path(course.dir,"course","stud_tokens"), cookie.name="courserStudLoginCookie", smtp=NULL, send.welcome.email=TRUE, ...) {
  restore.point("CoursePageApp")
  app = eventsApp()


  opts = init.th.opts(course.dir = course.dir)
  opts$courseid = courseid
  opts$token.dir = token.dir
  opts$cookie.name = cookie.name

  cp = as.environment(opts)
  cp$smtp = cp$email

  if (opts$has.pq) {
    cp$apq = init.apq(pq.dir=cp$pq.dir)
  }
  app$ui = fluidPage(
    mathjaxHeader(),
    if (opts$has.pq) pq.guess.headers(),
    uiOutput("mainUI")
  )

  if (is.null(app$glob$strings)) {
    string.file = system.file(file.path("forms",cp$lang,"strings.yaml"), package="courser")
    app$glob$strings = read.yaml(string.file)

  }
  if (is.null(app$glob$redraw.token.cr)) {
    file = system.file(file.path("forms",cp$lang,"redraw_token.Rmd"), package="courser")
    app$glob$redraw.token.cr = rmdtools::compile.rmd(file = file,out.type = "shiny")

  }



  app$glob$clicker.hs = compute.course.clicker.highscore(course.dir = course.dir)
  if (opts$has.pq) {
    app$glob$peerquiz.hs = compute.course.peerquiz.highscore(course.dir = course.dir)
  }

  app$glob$num.studs = length(unique(app$glob$clicker.hs$userid))

  app$cp = cp
  cp$cr = compile.coursepage(course.dir=course.dir, cp=cp)

  cp$send.welcome.email = send.welcome.email
  if (cp$send.welcome.email)
    coursepage.compile.welcome.email(cp=cp)

  smtp = first.none.null(smtp, list(from = opts$email$from,smtp = list(host.name = opts$email$smtpServer)))



  db.arg = list(dbname=paste0(login.db.dir,"/userDB.sqlite"),drv=SQLite())
  lop = loginModule(db.arg = db.arg, login.fun=coursepage.login, app.title=app.title,container.id = "mainUI",login.by.query.key = login.by.query.key, token.dir=token.dir, cookie.name="courserStudentLoginToken", smtp=smtp, app.url = opts$coursepage$url, ...)

  restore.point("CoursePageApp.with.lop")

  appInitHandler(function(...,app=getApp()) {
    initLoginDispatch(lop)
  })
  app
}


coursepage.login = function(userid=app$cp$userid,app=getApp(),tok=NULL,...) {
  restore.point("coursepage.login")

  cp = app$cp
  cp$userid = cp$email = userid

  db = cp$db = get.studentdb()

  cp$stud = dbGet(db,"students",params = nlist(userid), schemas = student.schemas())

  # save login in database
  dbInsert(db,"login_hist", vals=nlist(login_time=Sys.time(), userid=userid, showRanking=isTRUE(cp$stud$showRanking)),  schemas = student.schemas())


  # student does not yet exist
  # show modal settings window
  if (NROW(cp$stud) == 0 || isTRUE(!cp$stud$hasRegistered)) {
    coursepage.new.student.modals(cp=cp, app=app)
    return()
  }

  cp$stud = as.list(cp$stud)

  # Set cookie to login into clicker
  set.login.token.cookie(tok=list(userid=cp$stud$userid, key=cp$stud$token), "courserClickerCookie")

  show.coursepage()
}

coursepage.new.student.modals = function(cp, app=getApp()) {
  restore.point("coursepage.new.student.modals")

  db = cp$db
  if (NROW(cp$stud) == 0) {
    stud = list(userid=cp$userid,email=cp$email, nick=random.nickname(1))
  } else {
    stud = as.list(cp$stud[1,])
  }

  label = app$glob$strings$setting_btn


  settings.ui = student.settings.ui(cp=cp, submitBtn = actionButton("settingsModalBtn",label),values = stud)


  add.form.handlers(form=cp$settings.form,btn.id="settingsModalBtn",function(values,...)  {
      args = list(...)
      restore.point("settingsModalBtn")

      #res = dbGet(db, "students", params=values["nick"], schemas = student.schemas())
      #if (NROW(res)>0) {
      #  show.form.alert(form=form, msg=paste0("There is already a user with alias ", values$nick, ". Please pick another alias"))
      #}

      # Draw a random nickname
      #values$nick = paste0(courserClicker::random.nickname(1),"_", sample.int(10000,1))
      values$email = values$userid


      stud[names(values)] = values
      stud = student.default.aux.values(stud = stud)

      if (is.null(stud$token))
        stud$token = redraw.course.student.token(cp=cp,stud=stud)


      res = dbInsert(db, "students", stud, schemas=student.schemas())
      cp$stud = res$values
      dbInsert(db, "students_hist", c(list(mtime=Sys.time()),cp$stud), schemas=student.schemas())

      # Update restricted_login
      userhash = digest(cp$stud$userid)
      file = file.path(cp$clicker.dir,"restricted_login", userhash)
      if (isTRUE(cp$stud$simpleClickerLogin)) {
        if (file.exists(file)) file.remove(file)
      } else {
        if (!file.exists(file)) writeLines("block",file)
      }

      if(isTRUE(cp$send.welcome.email))
        coursepage.send.welcome.email(cp=cp, stud=stud)

      removeModal()

      set.login.token.cookie(tok=list(userid=stud$userid, key=stud$token), "courserClickerCookie")
      show.coursepage()
    })
    title = replace.whiskers(app$glob$strings$setting_title, list(courseid=cp$courseid, course.title=cp$course.title))

    # show first terms modal
    if (!isTRUE(stud$agreedTerms)) {
      ok.handler = function(...) {
        restore.point("termsOkHandler")
        stud$agreedTerms = TRUE
        showModal(modalDialog(
          settings.ui,
          title = title,
          easyClose = FALSE,footer = NULL
        ))
      }
      courser.show.terms.modal(course.dir=cp$course.dir, lang=cp$lang,ok.handler = ok.handler)
      return()
    }

    showModal(modalDialog(
      settings.ui,
      title = title,
      easyClose = FALSE,footer = NULL
    ))
    return()

}

show.coursepage = function(app=getApp(), cp=app$cp) {
  restore.point("show.coursepage")

  cp$cp.ui = rmdtools::render.compiled.rmd(cp$cr, envir=cp$stud,out.type = "shiny")
  cp$settings.ui = student.settings.ui(cp=cp,values = cp$stud,submit.handler = coursepage.submit.settings)
  cp$aux.settings.ui = student.aux.settings.ui(cp=cp,values = cp$stud,submit.handler = coursepage.submit.settings)

  ui = tabsetPanel(
    tabPanel(cp$courseid,cp$cp.ui),
    tabPanel(app$glob$strings$setting_tab,cp$settings.ui),
    tabPanel(app$glob$strings$aux_setting_tab,cp$aux.settings.ui)
  )

  setUI("mainUI",ui)
  dsetUI("mainUI",ui)


}

coursepage.submit.settings = function(values, app=getApp(),cp=app$cp,... ) {
  restore.point("coursepage.submit.settings")
  db = get.studentdb()

  stud = cp$stud
  stud[names(values)] = values
  res = dbInsert(db, "students", stud, schemas=student.schemas(),mode = "replace")
  dbInsert(db, "students_hist", c(list(mtime=Sys.time()),stud), schemas=student.schemas())

  cp$stud = res$values
  show.coursepage()
}

# need to put to external file at some point
student.default.aux.values = function(stud) {
  stud$defaultShowRanking = stud$showRanking = stud$emailRanking = sample(c(FALSE,TRUE),1)
  stud$simpleClickerLogin = TRUE
  stud
}


student.settings.ui = function(cp=app$cp, values = list(userid=cp$userid), submitBtn=NULL, submit.handler=NULL) {
  restore.point("student.settings.ui")
  lang = first.non.null(cp[["lang"]],"en")
  file = system.file(paste0("forms/",lang,"/student_settings.yaml"), package = "courser")
  cp$settings.form = yaml.form(file=file, lang=lang, prefix="stud_settings")
  ui = form.ui.simple(cp$settings.form, submitBtn=submitBtn,values = values,submit.handler = submit.handler)
  ui
}

student.aux.settings.ui = function(cp=app$cp, values = list(userid=cp$userid), submitBtn=NULL, submit.handler=NULL) {
  restore.point("student.aux.settings.ui")
  lang = first.non.null(cp[["lang"]],"en")
  file = system.file(paste0("forms/",lang,"/student_aux_settings.yaml"), package = "courser")
  cp$settings.form = yaml.form(file=file, lang=lang, prefix="stud_aux_settings")
  ui = form.ui.simple(cp$settings.form, submitBtn=submitBtn,values = values,submit.handler = submit.handler)
  token.ui = render.compiled.rmd(app$glob$redraw.token.cr, envir=list(token=cp$stud$token))
  tagList(
    ui,
    token.ui
  )
}



compile.coursepage = function(course.dir, page.file = file.path(course.dir,"course", "course_page.Rmd"), cp=app$cp, app=getApp()) {
  cr = rmdtools::compile.rmd(file = page.file)
  cr
}

coursepage_num_students = function(..., app=getApp(), cp=app$cp) {
  app$glob$num.studs
}

coursepage_current_tasks = function(...,cp=app$cp, app=getApp()) {
  restore.point("coursepage_current_tasks")
  if (isTRUE(cp$has.pq)) {
    ui = active.pqs.ui(cp$apq, userid=cp$userid)
  } else {
    ui = HTML("---")
  }
  ui
}

coursepage_closed_pq = function(...,cp=app$cp, app=getApp()) {
  if (isTRUE(cp$has.pq)) {
    ui = closed.pqs.ui(apq = cp$apq, userid=cp$userid)
  } else {
    ui = HTML("---")
  }
  ui
}




coursepage_clicker_ranking = function(...,width=480, height=280,cp=app$cp, app=getApp()) {
  args = list(...)
  restore.point("coursepage_ranglists")
  cat("\ncoursepage_ranglists")

  hs = app$glob$clicker.hs
  # user has not yet particpated
  if (length(which(hs$userid==cp$userid))==0)
    return(p("---"))

  clicker.svg = user.highscore.svg(hs, userid=cp$userid, lang=cp$lang, width=width, height=height)

  tagList(
    HTML(clicker.svg)
  )
}


coursepage_peerquiz_ranking = function(...,width=480, height=280,session.label="", cp=app$cp, app=getApp()) {
  args = list(...)
  restore.point("coursepage_peerquiz_ranking")

  hs = app$glob$peerquiz.hs
  # user has not yet particpated
  if (length(which(hs$userid==cp$userid))==0)
    return(p("---"))

  clicker.svg = user.highscore.svg(hs, userid=cp$userid, lang=cp$lang, width=width, height=height,session.label = session.label)

  tagList(
    HTML(clicker.svg)
  )
}


coursepage_start_clicker = function(label="Start Clicker",mode="buttonlink", app=getApp(), cp=app$cp) {
  restore.point("coursepage_start_clicker_button")

  token = cp$stud$token
  if (is.empty(token)) {
    token = redraw.course.student.token(cp=cp)
  }

  clicker.url = paste0(cp$clicker$url,"?key=",token)
  HTML(paste0('<a id="startClickerAppLink" href="',clicker.url,'" target="_blank" ', if (mode=="buttonlink") 'class="btn btn-default btn-xs"','>',label,'</a>'))

}

coursepage_homeslides = function(..., cp=app$cp, app=getApp()) {
  restore.point("coursepage_homeslides")

  last.dir = if (isTRUE(cp$local)) "local-home-slides" else "home-slides"

  dir = file.path(cp$course.dir,"course","shiny-server",last.dir)

  slides = list.files(dir)
  urls = paste0(cp$base_url,":",cp$homeslides$port,"/",slides,"?key=", cp$stud$token)

  html = paste0('<li><a href="',urls,'" target="_blank">',slides,'</a></li>', collapse="\n")
  html = paste0("<ul>\n", html,"\n</ul>")
  html
}

coursepage_redraw_token_button = function(label="New URL Code",msg="A new url code has been drawn: ") {
  ui = tagList(
    smallButton(id="redrawTokenBtn", label=label),
    uiOutput("redrawTokenMsg")
  )
  buttonHandler("redrawTokenBtn", function(...) {
    token = redraw.course.student.token(reset.links=TRUE)
    timedMessage("redrawTokenMsg",msg = paste0(msg,token),millis = Inf)
  })
  ui
}
is.empty = function(x, na.is.empty=TRUE) {
  if (length(x)==0) return(TRUE)
  if (na.is.empty & all(is.na(x))) return(TRUE)
  if (is.character(x) & nchar(x)==0) return(TRUE)
  FALSE
}

redraw.course.student.token = function(cp=app$cp, nchar=30, db=app$glob$studentdb, app=getApp(),reset.links=FALSE,stud=cp$stud, set.cookie = TRUE,...) {

  old.token = stud$token
  userid = stud$userid
  restore.point("redraw.course.student.token")

  if (!is.empty(old.token)) {
    # remove student token
    file = file.path(cp$token.dir,old.token)
    if (file.exists(file))
      file.remove(file)
  }

  # draw a token key
  tok = make.login.token(userid = userid, nchar.key = nchar)

  # save in db
  dbUpdate(db,table = "students",vals = list(token=tok$key),where = list(userid=cp$userid))

  # save in coursepage token dir
  write.login.token(tok=tok, token.dir=cp$token.dir)

  # set cookie that allows login
  # into home slides or clicker app
  # without url query key
  if (set.cookie)
    try(set.login.token.cookie(tok=tok,cp$cookie.name))

  if (!is.null(cp$stud))
    cp$stud$token= tok$key

  if (reset.links) {
    clicker.url = paste0(cp$clicker$url,"?key=",tok$key)
    callJS("$('#startClickerAppLink').attr","href",clicker.url)
    # TO DO: Reset links for slides

  }
  return(tok$key)
}

coursepage.send.welcome.email = function(cp, stud=cp$stud) {
  restore.point("coursepage.send.welcome.email")
  cr.li = cp$welcome.email.cr.li
  smtp = cp[["smtp"]]

  if (is.null(cr.li) | is.null(smtp)) return()

  email.enclos = c(stud,list(
    coursepage.url = cp$coursepage$url,
    coursepage.url.with.key = paste0(cp$coursepage$url,"?key=", stud$token),
    clicker.url = cp$clicker$url,
    clicker.url.with.key = paste0(cp$clicker$url,"?key=", stud$token),
    course.title = cp$course.title
  ))

  subject = render.compiled.rmd(cr.li$subject,envir = email.enclos)

  body = render.compiled.rmd(cr.li$body,envir = email.enclos)
  from = smtp$from
  control = list(smtpServer = smtp$smtpServer)

  # Try to send the welcome email
  try(sendmailR::sendmail(from=from, to=stud$email, subject=subject, msg = sep.lines(body), control=control))

}

coursepage.compile.welcome.email = function(cp, file = file.path(cp$course.dir, "course","settings","welcome_email.Rmd")) {
  restore.point("coursepage.compile.welcome.email")
  if (!file.exists(file)) return()

  txt = readUtf8(file,sep.lines = FALSE)
  li = parse.hashdot.yaml(txt)
  cr.li = lapply(li, function(txt) {
    compile.rmd(text = txt,fragment.only = TRUE,out.type = "md")
  })
  cp$welcome.email.cr.li = cr.li
  cr.li
}


