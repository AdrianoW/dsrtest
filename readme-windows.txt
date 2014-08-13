colin beginning
On windows, git is really slow because it's a bunch of scripts using unix fork().
Turns out there is no fork on windows, so they do all kind of hacks that makes it damn slow.

cygwin doesn't help.

The best option I've found is to use busybox; this is a single executable that contains most unix tools (ls, grep etc)
including git. This is damn fast, almost as fast as unix.
<<<<<<< HEAD
http://www.busybox.net/about.html cool stuff


It comes with mobaxterm. 
http://mobaxterm.mobatek.net/

This is what I use when I'm not logged into a VM. Pretty useful if you have to stay on windows.

Jose