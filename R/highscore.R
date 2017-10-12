examples.update.courser.highscore = function() {
  course.dir = "D:/libraries/courser/courses/vwl"
  dat = compute.course.peerquiz.highscore(course.dir)
  dat = compute.course.clicker.highscore(course.dir)

  svg = user.highscore.svg(dat,userid = "random_1",lang="de", session.label="Aufgabe")

  app = eventsApp()
  app$ui = tagList(
    h3("Ranking Peerquiz"),
    HTML(svg)
  )
  viewApp(app)

}

find.smallest.upper.bound.ind = function(x, vec, larger.as.na=TRUE) {
  restore.point("find.smallest.upper.bound")
  res = findInterval(x, vec, left.open=TRUE)+1
  if (larger.as.na) {
    res[res>length(vec)] = NA_integer_
  }
  res
}

clicker.adapt.data.for.home.sub = function(df) {

  # Home submissions will be mapped to the tag
  # with the next closest dat if such a tag exists
  sub.time = df %>%
    group_by(task.id, tag) %>%
    summarize(last.submit = max(submit.time)) %>%
    filter(tag != "home") %>%
    arrange(last.submit)

  sub.match = find.smallest.upper.bound.ind(df$submit.time, sub.time$last.submit)

  df$matched.tag = sub.time$tag[sub.match]
  df$from.home = df$tag == "home"


  df = filter(df, !(from.home & is.na(matched.tag)))

  # Set tag to matched tag for answers from home
  df$tag[df$from.home] = df$matched.tag[df$from.home]

  # Find which userid tag combinations
  # have ansers from the lecture
  df = df %>% group_by(task.id, tag, userid) %>%
    mutate(has.course.answer = any(!from.home)) %>%
    ungroup()

  # Remove from.home answers if there was an
  # answer in the lecture
  df = filter(df, !(has.course.answer & from.home))
  df
}

is.courser.clicker.highscore.up.to.date = function(course.dir) {
  hs.file = file.path(course.dir, "course","clicker","highscore","highscore.Rds")
  if ((!file.exists(hs.file))) return(FALSE)


  last.task.file = file.path(course.dir,"course","clicker","LAST_TASK.txt")
  if (!file.exists(last.task.file)) return(TRUE)


  task.time = file.mtime(last.task.file)
  hs.time = file.mtime(hs.file)

  task.time <= hs.time
}

get.course.clicker.highscore = function(course.dir, force.new=FALSE,...) {
  restore.point("get.course.clicker.highscore")
  up.to.date = is.courser.clicker.highscore.up.to.date(course.dir)
  if (!up.to.date | force.new) {
    compute.course.clicker.highscore(course.dir, ...)
  } else {
    df = readRDS(file.path(course.dir, "course","clicker","highscore","highscore.Rds"))
    as_data_frame(df)
  }

}

compute.course.clicker.highscore = function(course.dir, multi.tag.action="all", inflation.rate = 0.12, n.exp = 0.5, home.factor = 0.8, students=NULL, save=TRUE) {
  restore.point("compute.course.clicker.highscore")
  clicker.dir = file.path(course.dir,"course/clicker")
  df = update.all.aggregate.task.data(clicker.dir,return.data = TRUE)

  if (NROW(df)==0) return(NULL)

  df = clicker.adapt.data.for.home.sub(df)

  # create session.date and session.num
  df = df %>%
    group_by(task.id, tag) %>%
    mutate(session.date = as.Date(first(submit.time))) %>%
    ungroup()

  session.dates = sort(unique(df$session.date))

  # increasing session numbers
  df$session.num = match(df$session.date, session.dates)



  # only take last submission for a given tag
  # and weight home points
  df = group_by(df, task.id, tag, userid) %>%
    filter(submit.time==max(submit.time)) %>%
    ungroup() %>%
    mutate(points=mean(points)*ifelse(from.home,home.factor,1))

  # compute scores for each session
  sdf = df %>%
    group_by(session.num) %>%
    mutate(num.parts = n_distinct(task.id, part.ind)) %>%
    group_by(session.num, session.date, userid) %>%
    summarize(
      # adjust points by inflation and number of quiz parts in the session
      adj.points = sum(points*(1+inflation.rate)^(session.num-1) / num.parts^n.exp),
      points = sum(points),
    ) %>%
    ungroup()


  # Add students from DB
  # who have not entered any answer
  if (!is.null(students) & NROW(sdf)>0) {
    add.id = setdiff(students$userid,unique(sdf$userid))
    # Dummy entries with 0 points
    add.df = fast_df(session.num=sdf$session.num[1],session.date = sdf$session.date[1],userid=add.id,adj.points=0,points=0)
    sdf = rbind(sdf, add.df)
  }


  # filling missing observations with 0
  library(tidyr)
  sdf = tidyr::expand(sdf, nesting(session.num,session.date), userid  ) %>%
    left_join(sdf, by=c("session.num", "session.date", "userid")) %>%
    replace_na(list(points=0, adj.points=0)) %>%
    arrange(session.num, userid) %>%
    group_by(userid) %>%
    mutate(cum.points=cumsum(points), cum.adj.points=cumsum(adj.points)) %>%
    ungroup()

  # create ranks
  sdf = sdf %>%
    group_by(session.num) %>%
    mutate(
      rank = rank(-cum.adj.points, ties.method="average"),
      session.rank = rank(-points, ties.method="average")
      ) %>%
    ungroup() %>%
    arrange(session.num, rank) %>%
    group_by(userid) %>%
    mutate(rank.change = rank - lag(rank)) %>%
    ungroup()

  if (save) {
    saveRDS(sdf, file.path(course.dir, "course","clicker","highscore","highscore.Rds"))
  }

  sdf
}


compute.course.peerquiz.highscore = function(course.dir, inflation.rate = 0, pq.dir = file.path(course.dir,"course", "peerquiz"), tt = first.non.null(pqa[["tt"]],pq.load.time.table(pq.dir = pq.dir,convert.date.times = TRUE)), pq.li = pqa$pq.li,pqa=NULL, ...) {
  restore.point("compute.course.peerquiz.highscore")

  # find finished pq
  rows = tt$active & is.true(tt$end_guess <= Sys.time())
  ids = tt$id[rows]
  if (length(ids)==0) return(NULL)


  # compute points for every finished pq
  p.li = lapply(ids, pq.compute.points, pq.dir=pq.dir)
  df = bind_rows(p.li)

  # create session date time
  df = left_join(df, transmute(tt[rows,],id=id, session.date=end_guess),by="id")
  df$sort.id = paste0(as.integer(df$session.date),"__",df$id)
  sort.ids = sort(unique(df$sort.id))

  # increasing session numbers
  df$session.num = match(df$sort.id, sort.ids)

  # compute scores for each session
  sdf = df %>%
    mutate(adj.points = points*(1+inflation.rate)^(session.num-1))

  # filling missing observations with 0
  library(tidyr)
  sdf = tidyr::expand(sdf, nesting(session.num,session.date), userid  ) %>%
    left_join(sdf, by=c("session.num", "session.date", "userid")) %>%
    replace_na(list(points=0, adj.points=0)) %>%
    arrange(session.num, userid) %>%
    group_by(userid) %>%
    mutate(cum.points=cumsum(points), cum.adj.points=cumsum(adj.points)) %>%
    ungroup()

  # create ranks
  sdf = sdf %>%
    group_by(session.num) %>%
    mutate(
      rank = rank(-cum.adj.points, ties.method="average"),
      session.rank = rank(-points, ties.method="average")
      ) %>%
    ungroup() %>%
    arrange(session.num, rank) %>%
    group_by(userid) %>%
    mutate(rank.change = rank - lag(rank)) %>%
    ungroup()

  sdf
}



user.highscore.svg = function(dat, userid=NULL, return.bb = FALSE, width=480, height=320, session.label= if (lang=="de") "Vorlesung" else "Session",xlab=session.label, ylab="", lang="en", tooltip.fun=NULL) {
  restore.point("user.highscore.svg")

  library(bbsvg)

  dat = dat[dat$userid == userid,]

  x = dat$session.num
  rank = dat$rank

  yrange = c(-(round(max(rank)+0.5)), min(-1,-(round(min(rank)-0.6))))

  ticks = pretty(-(min(-yrange):max(-yrange)),n = 5,min.n = 2)
  ticks = ticks[ticks >= min(yrange) & ticks <= max(yrange)]
  tick.labels = - ticks

  if (is.null(tooltip.fun)) {
    if (lang=="de") {
      tooltip.fun = function(data,...) {
        paste0(session.label," ", data$session.num, "\nPunkte: ", round(data$adj.points,1),"\nRang: ", data$session.rank, "\nGesamtpunkte: ", round(data$cum.adj.points,1), "\nGesamtrang: ", data$rank)
      }
    } else {
      tooltip.fun = function(data,...) {
        restore.point("custom.tooltip.fun")
        paste0(session.label," ", data$session.num, "\nPoints: ", round(data$adj.points,1),"\nRank: ", data$session.rank, "\nTotal Points: ", round(data$cum.adj.points,1), "\nTotal rank: ", data$rank)
      }
    }
  }


  n = length(x)
  xrange = c(0.5, n+0.5)
  bb = bb_pane(show.ticks = TRUE,yrange=yrange,xrange=xrange,org.width = width, org.height=height) %>%
    bb_series(x = x,y=-rank, color="#000088", draw.points=TRUE,line.alpha=0.5) %>%
    bb_yaxis(show.grid = TRUE, show.line=TRUE,ticks=ticks, tick.labels=tick.labels, label=ylab) %>%
    bb_xaxis(ticks=x,label=xlab,labelpos = "center") %>%
    bb_series_tooltip_bars(tooltip.fun=tooltip.fun, tooltip.data = dat)
  if (n>1)  {
    bb= bb %>% bb_text(label=rank[-n], x=x[-n],y=-rank[-n],y.offset=10,font_size = 12,color="#000044",fill.background=TRUE, background.alpha=0.5)

  }
  bb = bb %>%
    bb_text(label=rank[n], x=x[n],y=-rank[n],y.offset=10,font_size = 16,color="#000044", fill.background=TRUE,, background.alpha=0.5)

  #bb
  #view.bb(bb)

  if (return.bb) {
    return(bb)
  }

  return(bb_to_svg(bb))
}
