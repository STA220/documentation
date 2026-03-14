hello <- function(who) {
  sprintf("Hello %s!", if (missing(who)) "world" else who)
}
hello("you")
