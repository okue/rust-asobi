{application, hello_app,
 [
  {description, "Hello app"},
  {vsn, "1"},
  {modules, [hello_app, hello_sup, hello]},
  {registered, [hello]},
  {applications, [kernel, stdlib]},
  {mod, {hello_app,[]}}
 ]
}.
