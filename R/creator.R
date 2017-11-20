#' An S4 class to represent objects.
#'
#' @slot type A character string indicating the object type.
#' @slot value The content of the object.

creator <- setClass(Class = "creator",
                 contains = "environment",
                   slots = c(identifier = 'character',
                             PropertyID = 'character',
                              firstName = 'character',
                               lastName = 'character',
                                   name = 'character',
                             createTime = 'character',
                                    uid = 'character'),

  validity = function(creator)
  {
    if (is.null(creator@PropertyID) | is.null(creator@identifier)) {
      return("Both an identifier and the identifier source must be included for a 'creator'")
    }

    if (is.null(creator@name) & is.null(creator@lastName)) {
      return("A creator must have either a name or a lastName.")
    }

    return(TRUE)
  }
)

setMethod("as.list", "creator", function(x) {
  list(identifier = x@identifier,
       PropertyID = x@PropertyID,
        firstName = x@firstName,
         lastName = x@lastName,
             name = x@name,
       createTime = x@createTime,
              uid = x@uid)
})

