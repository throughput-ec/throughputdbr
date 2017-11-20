#' An S4 class to represent objects.
#'
#' @slot type A character string indicating the object type.
#' @slot value The content of the object.

object <- setClass("object",
                    slots = c(type = 'character',
                             value = 'character',
                        createTime = 'character',
                               uid = 'character'),

  validity = function(object)
  {
   if (!object@type %in% c('DOI', 'URL', 'TextualBody', 'annotationText')) {
     return("The type must be one of DOI, URL or annotationText")
   }

  if(object@type == 'URL') {

    test_regex <- stringr::str_match(object@value,
                                     '(?i)\\b((?:[a-z][\\w-]+:(?:/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:\'".,<>?«»“”‘’]))')
    if(all(is.na(test_regex))) {
      return('If the object type is a URL it must have a valid URL/IRI.  The current value does not pass regex.')
    }
  }

  if(object@type == 'DOI') {
    test_regex <- stringr::str_match(object@value,
                                     '^(10[.][0-9]{4,}(?:[.][0-9]+)*/(?:(?!["&\'<>])\\S)+)$')
    if(all(is.na(test_regex))) {
      return('If the object type is a DOI it must have a valid DOI.  The current value does not pass regex.\nPlease ensure the DOI excludes the http address.')
    }
  }

  if(is.null(object@value)) {
    return('A generated body must contain content.')
  }

   return(TRUE)
 }
)

setMethod("as.list", "object", function(x) {
  list( type = x@type,
       value = x@value,
       uid   = x@uid,
       createTime = x@createTime)
})
