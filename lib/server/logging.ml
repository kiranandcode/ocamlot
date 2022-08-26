

let loggers = ref []

let add_logger l =
  let sub_logger = Dream.sub_log l in
  loggers := l :: !loggers;
  sub_logger

let set_log_level level =
  List.iter (fun l -> Dream.set_log_level l level) !loggers
