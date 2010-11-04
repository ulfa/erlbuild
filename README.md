# Erlbuild will be used for continous build and integration

# Phase 1

The following processes are implmented in a raw way:

- cc_timer is responsible for sending time triggered 
  events to the interface function time_triggered/1
- cc_file_poller is resonsible for polling a directory
  for changes. If there are any changes then he fires
  an event to the cc_controller
- cc_controller is responsible for compiling the
  changes source files. After the successful compiling
  the controller will send an event to the code_loader
- code_loader is responible for loading the changes 
  in from a specific directory. 

## Parameter

In this early version, i did the configuration in the 
erlbuild.app file.

- timer_clients : modules which implements the interface 
				  time_triggered/1
- svn_dir : directory where the svn checkout will be found
			(this is not supported yet)
- timer_interval : Time interval in msecond to the next run 
- files_regex : here you can configure the cc_file_poller 
				which kind of file he has to poll
- project_dir : this is root dir of the project that will be 
                observed
- polling_dir : is the directory which will be polled 
                by the cc_file_poller.
## Tested env

For the tests i used nitrogen, a web framework. Everytime
i edited and saved a file, the mechanism compiles and load
the changed files. So, no need to do something manually like
sync:all().

Steps to do:
- create the dir deps under nitrogen/Quickstarts
- edit the quickstart.sh and add the following:
  - ./deps/erlbuild/ebin to the path
  - -eval "application:start(erlbuild)" at the
    the end.
- change the configuration file (erlbuild.app)

- run quickstart.sh 
