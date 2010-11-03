{application,erlbuild,
             [{description,[]},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{erlbuild,[]}},
              {env,[
              		{timer_clients, [cc_file_poller]},
              		{svn_dir, ""},
              		{project_dir, "/Users/ulfangermann/projects/erlang/nitrogen/Quickstart"},
              		{polling_dir, "/Users/ulfangermann/projects/erlang/nitrogen/Quickstart/src"},
              		{compiler_options, [compile_all, {i, "./include"}, {i, "../apps"}, {i, "../apps/nitrogen/include"}, {outdir, "./ebin"}]},
              		{files_regex, ".*.erl$"},
              		{timer_interval, 1000}
              		]},
              {modules,[
              			code_reloader,erlbuild,
                        svnbootloader, cc_timer, 
                        cc_file_poller, cc_controller
                        ]}]}.
