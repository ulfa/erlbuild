# What is erlbuild and for what is it good?

erlbuild is a developer tool which reduce the cycle of comiling code, testing code and reloading code.
Everthing the developer has to do is to save his(her) source file and erlbuild will do the rest for you.

# Configuration

To configure erlbuild for your project, you only have to configure the dependency and start erlbuild.

## rebar.config

If you already have a rebar.config file, than insert the erlbuild dependency into the config file.

<code>
{deps, [
  {erlbuild, ".*", {git, "git@github.com:ulfa/erlbuild.git", "HEAD"}}
  ]}.
</code>

## How to start erlbuild

To start erlbuild you can do the following :

- in the shell : <code>application:start(erlbuild)</code>
- when you start the erlang shell <code>erl -pa $PWD/ebin $PWD/test $PWD/deps/*/ebin -boot start_sasl -s erlbuild</code> 

## ToDos

Before erlbuild will compile and reload the first time, you have to compile your sources once. After 
this, erlbuild will do the job for you.

Second issue. 

When the beam is not loaded, only compiled into the ebin directory and then you edit and make a mistake. 
The src file won't be compiled again, because the cc_file_poller is buggy.

