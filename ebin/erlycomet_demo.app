{application, erlycomet_demo,
  [
    {description, "Erlang Comet Server"},
    {vsn, "0.1"},
    {modules, [erlycomet_demo,
               erlycomet_demo_sup,
               erlycomet_demo_app,
			   erlycomet_demo_server,
               erlycomet_request,
			   erlycomet_api,
			   erlycomet_cluster,
               gen_server_cluster]},
    {registered, []},
    {applications, [kernel,
                    stdlib]},
    {included_applications, []},
    {env, []},
    {mod, {erlycomet_demo_app, []}} ]}.
    
     
