library(shinyapps)
dir.app <- paste0(Sys.getenv('PathGitHubRepos'),'/commute/app-commute')
print(dir.app)

#' Need to run the app at least once locally first so it caches the commute database into itself
shinyapps::deployApp(dir.app)
