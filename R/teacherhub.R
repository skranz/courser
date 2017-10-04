
examples.teacherhub = function() {
  restore.point.options(display.restore.point = TRUE)

  course.dir = "D:/libraries/courser/courses/vwl"
  app = TeacherHubApp(course.dir=course.dir,init.userid="sebastian.kranz@uni-ulm.de", need.password=FALSE, need.user=TRUE, fixed.password="test", use.signup=FALSE, validate.userid.fun=function(...) {return(list(ok=TRUE))})
  res = viewApp(app, port=app$glob$opts$teacherhub$port,launch.browser = rstudioapi::viewer)

  if (dir.exists(res)) {
    source(file.path(res,"app.R"))
    viewApp(app)
    #restore.point.options(display.restore.point = TRUE)
    #shiny::runApp(res,launch.browser = rstudioapi::viewer)
  }
}

# RTutor Teacher-Hub

# Teacher access for a single course
#
# a container for teachers
#   - teacherhub
#   - show problem sets
#
# one or several containers for students
#   - problem sets
#   - quizzes
#   - peerquizzes
#
# directory structure:
#
# course.dir
#   running
#   slides
#   ps
#   settings
#   shiny-server # need to specify in detail
#   clicker
#
#
#  a parent login database can be shared among different courses


TeacherHubApp = function(course.dir, courseid = basename(course.dir)
, login.db.dir=NULL, app.title=paste0("Teacherhub: ",courseid), token.dir = file.path(course.dir,"course", "teacher_tokens"), login.by.query.key="allow", smtp=NULL, ...) {
  restore.point("TeacherHubApp")
  app = eventsApp()

  app$ui = teacher.hub.main.ui()

  glob = app$glob
  glob$course.dir = app$course.dir = course.dir
  glob$courseid = app$courseid = courseid

  glob$token.dir = token.dir
  glob$opts = init.th.opts(course.dir)

  smtp = first.none.null(smtp, list(from = glob$opts$email$from,smtp = list(host.name = glob$opts$email$smtpServer)))

  db.arg = list(dbname=paste0(login.db.dir,"/userDB.sqlite"),drv=SQLite())


  lop = loginModule(db.arg = db.arg, login.fun=teacher.hub.login, app.title=app.title,container.id = "centerUI",token.dir = token.dir, login.by.query.key=login.by.query.key, allowed.userids=glob$opts$teachers, app.url = glob$opts$teacherhub$url, smtp=smtp, cookie.name=paste0("courserTeacherHub_",courseid), ...)

  restore.point("TeacherHubApp.with.lop")

  appInitHandler(function(...,app=getApp()) {
    restore.point("TeachHubApp.initHandler")
    hide.jquery.pane("mainPanes","west")
    initLoginDispatch(lop)
  })
  app
}

init.th.opts = function(course.dir, file = file.path(course.dir,"course/settings/settings.yaml")) {
  restore.point("init.th.opts")

  opts = yaml.load_file(file)
  opts$course.dir = course.dir

  if (isTRUE(opts$local)) {
    opts$base_url = "localhost"
    opts$clicker.dir = file.path(course.dir,"course", "clicker")
    opts$present.shiny.dir = file.path(course.dir,"course",  "shiny-server","present","slides")
  } else {
    opts$local = FALSE
    opts$clicker.dir = "/srv/clicker"
    opts$teachers.dir = "/srv/teachers"
    opts$present.shiny.dir = "/srv/present"
  }

  opts$teacherhub$url = paste0(opts$base_url,":",opts$teacherhub$port,"/th")

  opts$clicker$url = paste0(opts$base_url,":",opts$clicker$port,"/clicker")

  opts$coursepage$url = paste0(opts$base_url,":",opts$coursepage$port,"/coursepage")




  # peerquiz
  pq.dir = file.path(course.dir,"course","peerquiz")
  opts$has.pq = dir.exists(pq.dir)
  if (opts$has.pq) {
    opts$pq.dir = pq.dir
    set.pq.opts(init.pq.opts(pq.dir=pq.dir))
  }

  opts
}

teacher.hub.login = function(userid,app=getApp(),tok=NULL,lop=NULL,...) {
  restore.point("teacher.hub.login")

  course.dir = app$glob$course.dir
  courseid = first.non.null(app$courseid, basename(course.dir))
  opts = app$glob$opts


  # If we don't have a permanent tokens
  # specify a token for presenter login
  if (is.null(tok)) {
    # clean up temporary tokens from time to time
    if (runif(1)<0.3)
      remove.expired.login.tokens(app$glob$token.dir)
    tok = make.login.token(userid=userid,validMinutes = 60*24)
    write.login.token(tok, app$glob$token.dir)
  }

  app$tok = tok

  # Set a cookie to the login token such that we
  # autmotacially authenticate to a slide
  # presenterApp
  if (!is.null(opts$present.query.key)) {
    set.login.token.cookie(tok=list(key=opts$present.query.key),"courserPresenterCookie")
  } else {
    set.login.token.cookie(tok=tok,"courserPresenterCookie")
  }

  # Handlers to set and remove permament loginCookies
  buttonHandler("thSetCookieBtn", function(...) {
    tok = make.login.token(userid=userid,validMinutes = 60*24*365)
    write.login.token(tok, app$glob$token.dir)
    set.login.token.cookie(tok,lop$cookie.name, expires=365)
    timedMessage("thSettingsAlert","One year login cookie has been set.")
  })
  # Handlers to set and remove permament loginCookies
  buttonHandler("thRemoveCookieBtn", function(...) {
    removeCookie(lop$cookie.name)
    timedMessage("thSettingsAlert","One year login cookie has been removed.")
  })



  th = as.environment(nlist(
    userid,
    course.dir,
    courseid
  ))

  app$th = th
  show.teacher.hub.ui(th, app)
}

teacher.hub.main.ui = function(app=getApp()) {
  restore.point("show.teacher.hub.ui")

  json.opts ="
  defaults: {
    resizable: true,
    closable: false,
    slideable: true,
    spacing_open: 5
  },
  north: {
    size: 'auto',
    resizable: false,
    closable: false,
    slideable: false,
    spacing_open: 0
  },
  east: {
    resizable: true,
    spacing_open: 0,
    spacing_closed: 0,
    size: 0
  },
  west: {
    resizable: true,
    size: 0.5
  },

  "

  panes = jqueryLayoutPanes(id="mainPanes",json.opts=json.opts,
    north = div(p("TeacherHub"),thinHR()),
    west = div(uiOutput("westUI")),
    center = div(uiOutput("centerUI")),
    south = div(uiOutput("thAlert"))
  )

  ui = bootstrapPage(
    contextMenuHeader(),
    fancytreeHeader(extensions=c("table","gridnav","dnd")),
    aceEditorHeader(),
    jqueryLayoutHeader(),
    panes
  )

}

show.teacher.hub.ui = function(th=app$th,app=getApp()) {
  restore.point("show.teacher.hub.ui")

  treeId = "thTree"
  ns = NS(treeId)
  tree = fancy.file.tree(treeId,root.dir = th$course.dir,modify.nodes.fun = teacher.hub.modify.file.tree.nodes)

  setUI("westUI",tagList(
    filetreeButtons(treeId,c("Rename", "Duplicate","MakeDir","Delete")),
    tree
    ,filetreeButtons(treeId,c("Upload"))
  ))


  setUI("centerUI",HTML(""))
  center.ui = th.center.ui(th)
  setUI("centerUI",center.ui)
  #setUI("southUI",uiOutput("thAlerts"))

  show.jquery.pane("mainPanes",c("west","north","south"))

  classEventHandler("thShowSlidesBtn", event = "click",stop.propagation = TRUE, fun=th.show.slides.click)
  classEventHandler("thCompileSlidesBtn", event = "click",stop.propagation = TRUE, fun=th.compile.slides.click)
  classEventHandler("thEditFileBtn", event = "click",stop.propagation = TRUE, fun=th.edit.file.click)
  #setUI("westUI",tree)
  #setUI("centerUI",HTML(""))
}

th.center.ui = function(th,opts=app$glob$opts, app=getApp()) {
  restore.point("th.center.ui")

  tt.ui = if (opts$has.pq) div(style="padding-left: 1em",
    h3("Peerquiz Timetable"),
    pq.timetable.ui(pq.dir=opts$pq.dir)
  )
  settings.ui = tagList(
    simpleButton("thSetCookieBtn","Set one year login cookie"),
    simpleButton("thRemoveCookieBtn","Remove login cookie"),
    uiOutput("thSettingsAlert")
  )


  tabsetPanel(
    tabPanel("Time table", tt.ui),
    tabPanel("Settings", settings.ui)
  )


}

# change filetree nodes
teacher.hub.modify.file.tree.nodes = function(cur.dir, label.nodes, head.nodes, file.nodes,..., app=getApp(), th=app$th) {
  restore.point("teacher.hub.modify.file.tree.nodes")


  below = file.path.length(cur.dir) - file.path.length(th$course.dir)

  folders = rev(file.path.split(cur.dir))


  if (NROW(head.nodes)>0) {
    head.nodes$title = ".."
  }

  # main folder of a course
  if (below==0) {
    courseid = folders[1]
    app$courseid = courseid

    label.nodes$title = folders[1]
    label.nodes$col2 = label.nodes$col3 = ""

    file.nodes = file.nodes %>% arrange(desc(itemId))
    file.nodes$col2 = file.nodes$col3 = ""

    try({
      num.ps = length(list.dirs(recursive=FALSE,file.path(cur.dir,"ps")))
      num.slides = length(list.dirs(recursive=FALSE,file.path(cur.dir,"slides")))
      file.nodes$col2 <- c(num.slides, num.ps)
    })

  # slides
  } else if (below==1 & folders[1]=="slides") {
    label.nodes$title = paste0("Slides ", folders[2])
    label.nodes$col2 = label.nodes$col3 = ""


    file.nodes = file.nodes %>% filter(itemType=="folder")
    file.nodes$col2 = file.nodes$col3 = ""
    file.nodes$col2 <- sapply(file.nodes$itemId, function(slide) {
      as.character(smallButton(id=paste0("thShowSlides_",slide),class.add = "thShowSlidesBtn",label = "Show", `data-slide`=slide,`data-dir`=file.path(cur.dir,slide)))
    })
  # directly in a slide folder
  } else if (below==2 & folders[2]=="slides") {
    label.nodes$title = paste0("Slides ", folders[2])
    label.nodes$col2 = ""

    file.type = tolower(tools::file_ext(file.nodes$itemId))
    is.rmd = file.type == "rmd"

    file.nodes$col2[is.rmd] = sapply(file.nodes$itemId[is.rmd], function(file) {
      as.character(
        tagList(
          smallButton(id=paste0("thShowSlides_",file),class.add = "thShowSlidesBtn",label = "Show",`data-slide`=folders[1], `data-file`=file,`data-dir`=cur.dir),
          smallButton(id=paste0("thCompileSlides_",file),class.add = "thCompileSlidesBtn",label = "Compile", `data-file`=file,`data-dir`=cur.dir),
          smallButton(id=paste0("thEditFile_",file),class.add = "thEditFileBtn",label = "Edit", `data-file`=file,`data-dir`=cur.dir)
          )
        )
    })
  }


  nlist(label.nodes, head.nodes, file.nodes)
}

th.edit.file.click = function(data,..., courseid = app$courseid, app=getApp(), th=app$th) {
  restore.point("th.edit.file.click")
  file = data$file
  dir = data$dir
  th.show.edit.ui(file=file, dir=dir)
}

th.show.slides.click = function(data,..., courseid = app$courseid, app=getApp(), th=app$th) {
  restore.point("th.show.slides.click")
  file = data$file
  slides = data$slide
  slides.dir = data$dir

  opts = app$glob$opts
  app.dir = makePresenterAppDir(courseid=courseid,slides=slides,teacher=th$userid, hash="app", opts=opts, query.key=opts$present.query.key)

  url = paste0(opts$base_url,":", opts$present$port, "/slides/",slides)

  if (isTRUE(opts$local)) {
    stopApp(app.dir)
  } else {
    # open app website
    open.url.from.app(url)
  }

}



th.compile.slides.click = function(data,..., courseid = app$courseid, app=getApp(), th=app$th) {
  restore.point("th.compile.slides.click")
  file = data$file
  dir = data$dir
  long.file = file.path(dir, file)

  figure.dir = file.path(dir,"figure")
  if (!dir.exists(figure.dir)) {
    dir.create(figure.dir)
  }

  ret = try.with.warn({
    am = parse.armd(file = long.file, dir=dir,figure.dir = figure.dir)
    armd.to.ps(am = am,dir = dir, write.ps.rmd=FALSE, copy.into.global.env=FALSE)
  })
  if (is(ret,"error")) {
    th.alert(html = colored.html(as.character(ret$msg), color="red"))
  } else {
    th.alert("Successfully compiled.")
  }
}


shiny.to.js.html = function(txt,quotes='"') {
  txt = paste0(as.character(txt),collapse="")
  txt = gsub("\n","",txt, fixed=TRUE)
  if (isTRUE(quotes=="'")){
    txt = gsub('"',quotes,txt, fixed=TRUE)
  } else if (isTRUE(quotes=='"')) {
    txt = gsub("'",quotes,txt, fixed=TRUE)
  }
  txt
}

file.path.common = function(path1, path2) {
  restore.point("file.path.common")
  v1 = strsplit(path1,"",fixed = TRUE)[[1]]
  v2 = strsplit(path2,"",fixed = TRUE)[[1]]

  len = min(length(v1), length(v2))
  if (len==0) return("")
  differ = which(v1[1:len] != v2[1:len])
  if (length(differ)==0) {
    differ = len+1
  } else {
    differ = min(differ)
  }
  if (differ<=1) return("")
  substring(path1,1,differ-1)


}

file.path.diff = function(path1, path2) {
  restore.point("file.path.diff")

  common = file.path.common(path1, path2)

  if (nchar(common)==0) return(path1)
  str.right.of(path1,common)

}

th.alert = function(msg="",html=msg,ui=HTML(html), millis=Inf) {
  timedMessage(id="thAlert",ui=ui, millis=millis)
}

th.show.edit.ui = function(file, dir=NULL, app=getApp()) {
  restore.point("th.show.edit.ui")
  if (!is.null(dir)) file = file.path(dir, file)
  txt = read.as.utf8(file,sep.lines = FALSE)
  app$editFile = file
  ui = tagList(
    tags$h6(file),
    smallButton("thEditSave", "Save", form.ids = "thEditAce"),
  	HTML(aceEditorHtml("thEditAce",value = txt ,wordWrap = TRUE))
  )
  buttonHandler("thEditSave", function(...,formValues, app=getApp()) {
    txt = formValues[["thEditAce"]]
    file = app$editFile
    restore.point("thEditSaveClick")
    #writeUtf8(txt,file)
    writeLines(sep.lines(txt), file,useBytes = TRUE)
    cat("\nsave changes...")
  })
  setUI("centerUI", ui)
  dsetUI("centerUI", ui)

}
