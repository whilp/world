-- hooks configuration
-- each handler can be:
--   true              -> enabled with defaults
--   false             -> disabled
--   { enabled = bool, ... }  -> enabled with config options
return {
  post_commit_pr_reminder = true,
  session_start_bootstrap = true,
  session_start_make_help = true,
  stop_check_pr_file = true,
  stop_check_reminder = true,
}
