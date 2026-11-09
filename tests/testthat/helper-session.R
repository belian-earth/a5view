# A stand-in for a Shiny session: records registered data objects and
# session-end callbacks, and returns a data-object URL as Shiny does.
mock_session <- function() {
  env <- new.env()
  env$registered <- list()
  env$ended <- list()
  list(
    registerDataObj = function(name, data, filter) {
      env$registered[[name]] <- list(data = data, filter = filter)
      paste0("session/x/dataobj/", name, "?w=&nonce=", length(env$registered))
    },
    onSessionEnded = function(fn) env$ended <- c(env$ended, fn),
    env = env
  )
}
