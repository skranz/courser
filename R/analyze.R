do.analyze = function() {
  library(courser)

  course.dir = "D:/libraries/courser/courses/vwl"
  db = get.studentdb(course.dir = course.dir)

  students = dbGet(db,"students")
  hs = get.course.clicker.highscore(course.dir=course.dir, force.new=TRUE)
  hs = left_join(hs, students, by="userid")

  hs = hs %>% group_by(userid) %>%
    mutate(
      final.points = max(cum.adj.points),
      final.rank = sum(rank* (session.num==max(session.num)))
    ) %>%
    ungroup() %>%
    arrange(-session.num, final.rank)

  library(ggplot2)

  d = filter(hs, final.rank <= 40)

  ggplot(data=d, aes(x=session.num,y=-rank, color=subject)) + geom_line() + facet_wrap(~final.rank)

  ggplot(data=d, aes(x=session.num,y=-rank, color=gender)) + geom_line() + facet_wrap(~final.rank)

  ggplot(data=d, aes(x=session.num,y=-rank, color=allowGrade)) + geom_line() + facet_wrap(~final.rank)

}
