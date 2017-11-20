#  Add annotation that Goring knows all datasets were included from Blois et al.
#' @title Link Records
#' @description Links records within the graph database using an annotation model.
#' @author Simon Goring
#' @param generator A list containing (at minimum) a single element, representing a person.
#' @param body An (optional) object of class \code{object}, with parameters \code{type} and \code{body}.
#' @param body_rel An (optional) object of class \code{object}, which acts as a parent for the body element, for example, a database from which the \code{body} is drawn.
#' @param target The \code{object} that is being annotated.  May be a single object or a list of \code{object}s.
#' @param target_rel An (optional) object of class \code{object}, which acts as a parent for the target element, for example, a database from which the \code{target} is drawn.
#' @description This function generates a graph that links bodies to targets, and adds the resources from which the post is made.
#' @return A \code{list} with values \code{create_time} and \code{uid}.
#' @importFrom assertthat::assert_that
#' @import RNeo4j
#'
link_record <- function(con,
                        generator,
                        body = NULL,
                        body_rel = NULL,
                        target,
                        source = NULL,
                        body_composite = FALSE) {

  #  Prior assertions to check the parameters of the objects have been changed to be within the
  #  class definition of the classes.

  assertthat::assert_that(is(generator, "creator") | is(generator, 'list'))
  assertthat::assert_that(is(body, "object") | is.null(body) | is(body, 'list'))
  assertthat::assert_that(is(body_rel, "object") | is.null(body_rel))
  assertthat::assert_that(is(target, "object") | is(body, 'list'))
  assertthat::assert_that(is(source, "object") | is.null(source))

  neo_trans <- newTransaction(con)

  create_time <- as.character(format(Sys.time(), "%F %T %z"))

  uid <- formatC(round(runif(1, min = 0, max = 1e7)),
                 width = 7, format = "d", flag = "0")

  # We know, for W3C that any annotation node has the type: oa:Annotation
  # Allows for one or multiple creators.  Currently no author order is supported, but
  # could be added as an "order" parameter in the `created` field.

  query_cre <- "MERGE  (ann:annotation {created:{createTime}, uid:{uid}})
                MERGE  (cre:creator {id: {identifier},
                                     PropertyID:{PropertyID},
                                     firstName:{firstName},
                                     lastName:{lastName}})
                MERGE (cre)-[:created]->(ann)"

  if (class(generator) == 'creator') {
    generator@createTime <- create_time
    generator@uid        <- uid

    input <- as.list(unlist(c(uid = uid, createTime = create_time, as.list(generator))))

    appendCypher(neo_trans, query_cre, input)

  } else {
    for (i in 1:length(generator)) {
      generator[[i]]@createTime <- create_time
      generator[[i]]@uid        <- uid

      appendCypher(neo_trans,
                   query_cre,
                   as.list(generator[[i]]))
    }
  }

  # Add the body element, one or multiple supported.  As per the W3C parameters there
  # is no order in the body elements.

  add_body <- "MERGE (bod:object {type: {type}, value:{value}})
               MERGE (ann:annotation {created:{createTime}, uid:{uid}})
               MERGE (bod)<-[:hasBody]-(ann)"

  if (!is.null(body)) {
    if (class(body) == 'object') {

      input <- as.list(unlist(c(uid = uid,
                                createTime = create_time,
                                as.list(body))))

      appendCypher(neo_trans,
                   add_body,
                   input)

    } else {
      for (i in 1:length(body)) {
        body[[i]]@createTime <- create_time
        body[[i]]@uid        <- uid

        input <- as.list(unlist(c(uid = uid,
                                  createTime = create_time,
                                  as.list(body[[i]]))))

        appendCypher(neo_trans,
                     add_body,
                     input)
      }
    }
  }

  add_target <- "MERGE (tar:object {type: {type}, value:{value}})
                 MERGE (ann:annotation {created:{createTime}, uid:{uid}})
                 MERGE (tar)<-[:hasTarget]-(ann)
                 MERGE (res:object {type: {type_r}, value:{value_r}, class: 'Resource'})
                 MERGE (tar)-[:hasRes]->(res)"

  if (!is.null(target)) {
    if (class(target) == 'object') {

      target@createTime  <- create_time
      target@uid         <- uid

      input <- as.list(unlist(c(type_r = source@type,
                                value_r = source@value,
                                as.list(target))))

      appendCypher(neo_trans,
                   add_target,
                   input)

    } else {
      for (i in 1:length(target)) {
        target[[i]]@createTime <- create_time
        target[[i]]@uid         <- uid

        input <- as.list(unlist(c(type_r = source@type,
                                  value_r = source@value,
                                  as.list(target[[i]]))))

        appendCypher(neo_trans,
                     add_target,
                     input)
      }
    }
  }

  commit(neo_trans)

  return(list(create_time, uid))
}
