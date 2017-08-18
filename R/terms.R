example.courser.terms = function() {
  app = eventsApp()
  app$ui = fluidPage(
    p("Hello World")
  )
  appInitHandler(function(...) {
    courser.show.terms.modal(lang="de", ok.handler=function(...) cat("\nok continue..."))
  })
  viewApp(app)
}

courser.terms.ui = function(course.dir=NULL, lang="en") {
  restore.point("courser.terms.ui")

  file = system.file(paste0("forms/",lang,"/terms.Rmd"), package = "courser")

  txt = readUtf8(file, sep.lines = FALSE)
  li = parse.hashdot.yaml(txt)


  tagList(
    HTML(paste0('<textarea wrap="soft" cols="80" readonly style="max-width: 100%; height: 70vh">',li$terms,'</textarea>')),
    checkboxInput("termsAcceptCheckBox",label = li$checkBoxLabel, value=FALSE),
    submitButton("termsContinueBtn", li$btnLabel, form.ids="termsAcceptCheckBox"),
    div(id="termsNoAcceptMsg", style="font-color: red; display: none", HTML(li$noCheckMsg))
  )
}



courser.show.terms.modal = function(course.dir = NULL, lang="en", ui = courser.terms.ui(course.dir=course.dir, lang=lang), ok.handler=function(...){}) {
  restore.point("courser.show.terms.modal")
  buttonHandler("termsContinueBtn", function(formValues,...){
    restore.point("termsContinueBtn")
    accept = formValues$termsAcceptCheckBox
    if (!accept) {
      evalJS('$("#termsNoAcceptMsg").show();')
      return()
    }
    removeModal()
    ok.handler()
  })

  showModal(modalDialog(
    ui,
    title = "",
    easyClose = FALSE,footer = NULL
  ))

}
