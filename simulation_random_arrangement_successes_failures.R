success <- 21
attempt <- 30
fail <- attempt - success
streak <- function()
{
  success.pos <- sort(sample(1:attempt, success))
  fail.pos <- sort((1:attempt)[-success.pos])
  streak <- 0
  prev <- success.pos[1]
  for(i in success.pos[-1])
  {
    current <- i
    if(prev == current-1)
    {
      streak <- streak + 1
    }
    prev <- current
  }
  
  prev <- fail.pos[1]
  for(i in fail.pos[-1])
  {
    current <- i
    if(prev == current-1)
    {
      streak <- streak + 1
    }
    prev <- current
  }
  return(streak/attempt)
}