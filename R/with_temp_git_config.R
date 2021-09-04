with_temp_git_config <- function(code,
                                 user.name = "user",
                                 user.email = "user@email.com"){

  on.exit(invisible())

  if (!git_user_name_exists()) {
    git2r::config(global = TRUE, user.name = user.name)
    on.exit(git2r::config(global = TRUE, user.name = NULL), add = TRUE)
  }

  if (!git_user_email_exists()) {
    git2r::config(global = TRUE, user.email = user.email)
    on.exit(git2r::config(global = TRUE, user.email = NULL), add = TRUE)
  }

  eval(code)

}
