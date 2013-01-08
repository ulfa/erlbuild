# Erlbuild will be used for continous build 



The following processes are implmented :

- cc_timer is responsible for sending time triggered events to the interface function time_triggered/1
- cc_file_poller is resonsible for polling a directory for changes. If there are any changes then he fires an event to the cc_controller
- cc_compiler is responsible for compiling the changes source files. After the successful compiling the compiler will send an event to the cc_reloader
- cc_reloader is responible for loading the changes  in from a specific directory. 

