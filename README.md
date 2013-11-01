SpringLobbyBot
==============
Perl templates to bootstrap [SpringRTS](http://springrts.com/) lobby bot
development.

Components
----------
* [SpringLobbyBot.pl](SpringLobbyBot.pl): Lobby bot application template
* [SpringLobbyBotConf.pm](SpringLobbyBotConf.pm): Template module for bot
  configuration management.
* [etc/commands.conf](etc/commands.conf): Configuration template for commands
  rights
* [etc/levels.conf](etc/levels.conf): Configuration template for access levels
* [etc/SpringLobbyBot.conf](etc/SpringLobbyBot.conf): Global lobby bot
  configuration template
* [etc/users.conf](etc/users.conf): Configuration template for user access
  rights
* [var/help.dat](var/help.dat): Commands help template for user access rights

Dependencies
------------
The SpringLobbyBot application depends on following projects:
* [SimpleLog](https://github.com/Yaribz/SimpleLog)
* [SpringLobbyInterface](https://github.com/Yaribz/SpringLobbyInterface)

Usage
-----
* Copy the dependencies listed above ([SimpleLog.pm](https://raw.github.com/Yaribz/SimpleLog/master/SimpleLog.pm)
  and [SpringLobbyInterface.pm](https://raw.github.com/Yaribz/SpringLobbyInterface/master/SpringLobbyInterface.pm))
  into same location as SpringLobbyBot files
* Edit the etc/SpringLobbyBot.conf file to set following parameters: lobbyLogin
  (name of the lobby account used by the bot), lobbyPassword (password of the
  lobby account used by the bot), etcDir (directory containing bot config files
  ), varDir (directory containing bot dynamic data), logDir (directory
  containing bot log files)
* Replace &lt;ownerAccountId&gt; and &lt;ownerName&gt; by your actual Spring lobby account
  ID and name (not those of the bot) in etc/users.conf
* Edit the SpringLobbyBot.pl and SpringLobbyBotConf.pm files and replace
  "SpringLobbyBot" by the true name of your bot application
* Rename following files according to your bot application name:
  SpringLobbyBot.pl, SpringLobbyBotConf.pm, etc/SpringLobbyBot.conf
* Launch the bot using command "./SpringLobbyBot.pl etc/SpringLobbyBot.conf"
  (replace "SpringLobbyBot" by the name chosen in previous steps)

Examples
--------
Following projects are known to be based on SpringLobbyBot templates:
* [SPADS](https://github.com/Yaribz/SPADS)
* [SpringLobbyStats](https://github.com/Yaribz/SpringLobbyStats)
* [SLDB](https://github.com/Yaribz/SLDB)
* [BuildServ](https://github.com/Yaribz/BuildServ)

Licensing
---------
Please see the file called [LICENSE](LICENSE).

Author
------
Yann Riou <yaribzh@gmail.com>