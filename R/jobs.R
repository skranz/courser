examples.finde.courser.jobs = function() {
  restore.point.options(display.restore.point=TRUE)
  course.dir = "D:/libraries/courser/courses/vwl"

  jo = cojo_init(course.dir=course.dir) %>%
    cojo_find() %>%
    cojo_perform() %>%
    cojo_send_emails()

  jo$em.li
  cat(jo$email.txt)

  jobs = find.courser.jobs(course.dir)
  perform.courser.jobs(jobs, course.dir=course.dir)


}

# load and store data about jobs
cojo_init = function(jo=NULL,course.dir=jo$course.dir, db = get.studentdb(course.dir)) {
  restore.point("cojo_init")


  schema.file = system.file("schema/studentdb.yaml", package="courser")
  schemas = load.and.init.schemas(schema.file)

  #li = parse.hashdot.yaml(txt)
  em.cr.li = compile.courser.email.templates(course.dir=course.dir)

  settings = course.settings(course.dir)

  students = dbGet(db,"students", schemas=schemas)

  students$url.clicker = paste0(settings$base_url,":",settings$clicker$port,"/clicker?code=", students$token)

  students$url.coursepage = paste0(settings$base_url,":",settings$coursepage$port,"/coursepage?code=", students$token)

  jo = list(
    course.dir = course.dir,
    settings = settings,
    db = db,
    schemas=schemas,
    old.jobs = dbGet(db,"jobs",schemas = schemas),
    students = students,
    send.email = rep(FALSE, NROW(students)),
    em.li = list(),
    em.cr.li = em.cr.li,
    time = Sys.time()
  )
  jo$course.title = first.non.null(jo$settings$course.title,basename(course.dir))

  email.enclos = as.environment(list(
    num.stud = NROW(students),
    course.title = jo$course.title
  ))
  parent.env(email.enclos) = parent.frame()
  jo$email.enclos = email.enclos

  jo
}

# search for automatic jobs like updating the highscore
# and sending emails
cojo_find = function(jo) {
  restore.point("cojo_find")
  course.dir = jo$course.dir
  jobs = NULL

  # check if there was run new clicker task tag
  # since the highscore was last send
  last.task.file = file.path(course.dir,"course","clicker","LAST_TASK.txt")

  has.task = file.exists(last.task.file)
  if (has.task) {
    last.email.file = file.path(course.dir,"course","clicker","LAST_EMAIL.txt")
    if (!file.exists(last.email.file)) {
      jo$last.email.clicker.session.num = 0
      jobs = c(jobs,"hs_clicker")

    } else {
      task.time = file.mtime(last.task.file)
      email.time = file.mtime(last.email.file)
      if ((task.time > email.time)) {
        jo$last.email.clicker.session.num = as.integer(readLines(last.email.file))
        jobs = c(jobs,"hs_clicker")
      }
    }
  }
  jo$jobs = jobs
  jo
}

cojo_perform = function(jo) {
  restore.point("perform.courser.jobs")
  course.dir = jo$course.dir; jobs = jo$jobs

  if ("hs_clicker" %in% jobs) {
    hs = compute.course.clicker.highscore(course.dir=course.dir)

    # specify sessions that will be included in the emails
    session.nums = unique(hs$session.num)
    session.nums = session.nums[session.nums > jo$last.email.clicker.session.num]
    # create email content for each clicker sessions
    for (session.num in sort(session.nums)) {
      jo = cojo.clicker.highscore.email(jo,hs,session.num)
    }

    jo$hs_clicker = hs
  }

  jo = cojo.make.full.emails(jo)
  jo
}

cojo.clicker.highscore.email = function(jo, hs, session.num) {
  restore.point("cojo.clicker.highscore.email")

  # create df to evaluate
  hs=hs[hs$session.num == session.num,]
  hs$userid = tolower(hs$userid)
  df = left_join(jo$students, hs,by="userid")
  #df$course.title = jo$course.title
  #df$num.stud = NROW(jo$students)

  str = rep("", NROW(jo$students))

  rows = jo$students$emailRanking
  if (sum(rows)>0) {
    str[rows] = render.vectorized.compiled.rmd(jo$em.cr.li$quiz_ranking, df[rows,],enclos = jo$email.enclos)
  }

  jo$em.li[[paste0("quiz_ranking_", session.num)]] = str
  jo$send.email = jo$send.email | nchar(str)>0
  jo

}

cojo.make.full.emails = function(jo) {
  restore.point("cojo.make.full.emails")
  rows = which(jo$send.email)
  n = length(rows)
  jo$email.txt = jo$email.title = header = footer = rep("", NROW(jo$students))
  if (n==0) return(jo)

  df = jo$students[rows,]

  jo$email.subject[rows] = render.vectorized.compiled.rmd(jo$em.cr.li$subject, df,enclos = jo$email.enclos)

  header[rows] = render.vectorized.compiled.rmd(jo$em.cr.li$header, df,enclos = jo$email.enclos)
  footer[rows] = render.vectorized.compiled.rmd(jo$em.cr.li$footer, df,enclos = jo$email.enclos)

  str = do.call(paste0, c(list(header),jo$em.li,list(footer)))
  jo$email.txt = str

  cat(str)
  jo
}


cojo_send_emails = function(jo) {
  rows = which(jo$send.email)

  from = jo$settings$from
  control = list(smtpServer = jo$settings$smtpServer)
  for (row in rows) {
    stud = as.list(jo$students[row,])
    if (!has.substr("@",stud$email)) {
      cat("\nskip email for ", stud$email, " since that is no vald email adress.")
      next
    }
    cat("\nSend email to ", stud$email, " with title ",jo$email.subject[row],"...")
    sendmailR::sendmail(from=from, to=stud$email, subject=jo$email.subject[row], msg = jo$email.txt[row], control=control)
  }

}


compile.courser.email.templates = function(file = file.path(course.dir, "course","settings","emails.Rmd"), course.dir=NULL) {
  restore.point("compile.courser.email.templates")
  txt = readUtf8(file,sep.lines = FALSE)
  #cr = compile.rmd(text=txt,fragment.only = TRUE,out.type = "md")

  li = parse.hashdot.yaml(txt)
  cr.li = lapply(li, function(txt) {
    compile.rmd(text = txt,fragment.only = TRUE,out.type = "md")
  })
  cr.li
}