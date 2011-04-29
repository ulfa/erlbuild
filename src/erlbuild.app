{application,erlbuild,
             [{description,[]},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{erlbuild,[]}},
              {env,[
              		{timer_clients, [cc_file_poller]},
              		{svn_dir, ""},
              		{project_dir, "."},
              		{polling_dir, "./src"},
              		{compiler_options, [compile_all, {i, "./include"}, {outdir, "./ebin"}]},
              		{files_regex, ".*.erl$"},
              		{timer_interval, 10000}
              		]},
              {modules,[
              			code_reloader,erlbuild,
                        svnbootloader, cc_timer, 
                        cc_file_poller, cc_controller
                        ]}]}.
