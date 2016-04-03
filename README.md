esc - Erlang Structure Checker
==============================

Problem:
To check that data stored in erlang term format conforms to some
required structure.  In erlang applications it is common to store
configuration data in text files, and read it in with a file:consult
command.  This is a potential source of bugs.

The Solution:
Configuration files are checked against definition expression.

The term definition language is discussed in:  
http://kiwichristianarchy.blogspot.com/2016/03/taming-configuration-files-general-term.html

For a functioning example, check out the example directory.
