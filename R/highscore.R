examples.update.courser.highscore = function() {
  course.dir = "D:/libraries/courser/courses/vwl"
  dat = compute.course.clicker.highscore(course.dir)

  svg = user.highscore.svg(dat,lang="de")
}

compute.course.clicker.highscore = function(course.dir, multi.tag.action="all", inflation.rate = 0.12, n.exp = 0.5) {
  restore.point("compute.course.clicker.highscore")
  clicker.dir = file.path(course.dir,"course/clicker")
  df = update.all.aggregate.task.data(clicker.dir,return.data = TRUE)

  # create session.date and session.num
  df = df %>%
    group_by(task.id, tag) %>%
    mutate(session.date = as.Date(first(submit.time))) %>%
    ungroup()

  session.dates = sort(unique(df$session.date))

  # increasing session numbers
  df$session.num = match(df$session.date, session.dates)

  # Just for testing reasons to generate some different
  # session numbers
  df$session.num = match(df$task.id, unique(df$task.id))

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


  # filling missing observations with 0
  library(tidyr)
  sdf = tidyr::expand(sdf, nesting(session.num,session.date), userid  ) %>%
    left_join(sdf, by=c("session.num", "session.date", "userid")) %>%
    replace_na(list(points=0)) %>%
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

  # random data
  nse = 10
  x = 1:nse
  gain = sample(-20:20, nse, replace=TRUE)
  dat=data_frame(session.num=x, rank=50+gain, session.rank=gain, adj.points=6, cum.adj.points=20)




  x = dat$session.num
  rank = dat$rank

  yrange = c(-(round(max(rank)+0.5)), min(-1,-(round(min(rank)-0.6))))

  ticks = pretty(-(min(-yrange):max(-yrange)),n = 5,min.n = 2)
  ticks = ticks[ticks >= min(yrange) & ticks <= max(yrange)]
  tick.labels = - ticks

  if (is.null(tooltip.fun)) {
    if (lang=="de") {
      tooltip.fun = function(data,...) {
        restore.point("hdfkdhf")
        paste0(session.label," ", data$session.num, "\nPunkte: ", data$adj.points,"\nRank: ", data$session.rank, "\nGesamtpunkte: ", data$cum.adj.points, "\nGesamtrank ", data$rank)
      }
    } else {
      tooltip.fun = function(data,...) {
        restore.point("custom.tooltip.fun")
        paste0(session.label, data$session.num, "\nRank ", data$rank)

      }

    }
  }


  n = length(x)
  bb = bb_pane(show.ticks = TRUE,yrange=yrange,org.width = width, org.height=height) %>%
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
