{application, erlcounter, 
  [{vsn, "1.0.0"},
   {modules, []},
   {registered, []},
   {mod, {erlcounter, []}},
   {env, [{directory, "."},
           {regex, ["if\\s.+->", "case\\s.+\\sof"]},
           {max_files, 10}]}
  ]}.