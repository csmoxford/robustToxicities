############################################################################################
# some simple ui functions to reduce duplication.
textInputRow<-function (inputId, label, value = "") {
  div(
    style="display:block",
    tags$label(label, `for` = inputId),
    tags$input(id = inputId, type = "text", value = value,class="col-lg-12")
  )
}

numericInputRow_small<-function (inputId, label, value = "") {
  div(
    style="display:inline-block",
    p(tags$label(label, `for` = inputId)),
    p(tags$input(id = inputId, type = "number", value = value))
  )
}

numericInput_small<-function (inputId,label, value = "",...){
  div(
    class="form-group",style="display:block; text-align:right",
    tags$label(label, `for` = inputId,class="col-lg-9",style="padding-bottom:10px;"),
    tags$input(id = inputId, type = "number", value = value,class="col-lg-3",...)
  )
}

buttonInput<-function(id,class,name,...){
  tags$button(
    id=id,
    type="button",
    class=class,
    style="width:200px;",
    HTML(name),
    ...
  )
}
