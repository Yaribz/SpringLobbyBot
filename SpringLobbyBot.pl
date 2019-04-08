#!/usr/bin/perl -w
#
# SpringRTS lobby bot template (replace "SpringLobbyBot" by the actual bot name in this file and remove this line)
# SpringRTS lobby bot: SpringLobbyBot
#
# Copyright (C) 2013  Yann Riou <yaribzh@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


use strict;

use POSIX (':sys_wait_h','ceil');

use IO::Select;
use Text::ParseWords;

use SimpleLog;
use SpringLobbyBotConf;

use SpringLobbyInterface;

my $SpringLobbyBotVer='0.2';

$SIG{TERM} = \&sigTermHandler;
$SIG{USR1} = \&sigUsr1Handler;
$SIG{USR2} = \&sigUsr2Handler;

my %ircColors;
my %noColor;
for my $i (0..15) {
  $ircColors{$i}=''.sprintf('%02u',$i);
  $noColor{$i}='';
}
my @ircStyle=(\%ircColors,'');
my @noIrcStyle=(\%noColor,'');

my $win=$^O eq 'MSWin32' ? 1 : 0;

my %lobbyHandlers = ( help => \&hHelp,
                      helpall => \&hHelpAll,
                      quit => \&hQuit,
                      reloadconf => \&hReloadConf,
                      restart => \&hRestart,
                      version => \&hVersion );

# Basic checks ################################################################

if($#ARGV != 0 || ! (-f $ARGV[0])) {
  print "usage: $0 <configurationFile>\n";
  exit 1;
}

my $confFile=$ARGV[0];
my $sLog=SimpleLog->new(prefix => "[SpringLobbyBot] ");
my $botConf=SpringLobbyBotConf->new($confFile,$sLog);

sub slog {
  $sLog->log(@_);
}

if(! $botConf) {
  slog("Unable to load SpringLobbyBot configuration at startup",0);
  exit 1;
}

my $masterChannel=$botConf->{conf}->{masterChannel};
$masterChannel=$1 if($masterChannel =~ /^([^\s]+)\s/);

# State variables #############################################################

my %conf=%{$botConf->{conf}};
$sLog=$botConf->{log};
my $lSock;
my @sockets=();
my $running=1;
my $quitScheduled=0;
my %timestamps=(connectAttempt => 0,
                ping => 0);
my $lobbyState=0; # (0:not_connected, 1:connecting, 2: connected, 3:logged_in, 4:start_data_received)
my %pendingRedirect;
my $p_answerFunction;
my $lobbyBrokenConnection=0;
my %lastSentMessages;
my @messageQueue=();
my @lowPriorityMessageQueue=();
my %lastCmds;
my %ignoredUsers;
my $triedGhostWorkaround=0;
my $lanMode=0;

my $lobbySimpleLog=SimpleLog->new(logFiles => [$conf{logDir}."/SpringLobbyBot.log"],
                                  logLevels => [$conf{lobbyInterfaceLogLevel}],
                                  useANSICodes => [0],
                                  useTimestamps => [1],
                                  prefix => "[SpringLobbyInterface] ");

my $lobby = SpringLobbyInterface->new(serverHost => $conf{lobbyHost},
                                      serverPort => $conf{lobbyPort},
                                      simpleLog => $lobbySimpleLog,
                                      warnForUnhandledMessages => 0);

# Subfunctions ################################################################

sub sigTermHandler {
  scheduleQuit('SIGTERM signal received');
}

sub sigUsr1Handler {
  scheduleQuit('SIGUSR1 signal received',2);
}

sub sigUsr2Handler {
  my $newSpringLobbyBot=SpringLobbyBotConf->new($confFile,$sLog);
  if(! $newSpringLobbyBot) {
    slog('Unable to reload SpringLobbyBot configuration',2);
  }else{
    $botConf=$newSpringLobbyBot;
    %conf=%{$botConf->{conf}};
    slog('SpringLobbyBot configuration reloaded',3);
  }
}

sub forkedError {
  my ($msg,$level)=@_;
  slog($msg,$level);
  exit 1;
}

sub secToTime {
  my $sec=shift;
  my @units=qw/year day hour minute second/;
  my @amounts=(gmtime $sec)[5,7,2,1,0];
  $amounts[0]-=70;
  my @strings;
  for my $i (0..$#units) {
    if($amounts[$i] == 1) {
      push(@strings,"1 $units[$i]");
    }elsif($amounts[$i] > 1) {
      push(@strings,"$amounts[$i] $units[$i]s");
    }
  }
  @strings=("0 second") unless(@strings);
  return $strings[0] if($#strings == 0);
  my $endString=pop(@strings);
  my $startString=join(", ",@strings);
  return "$startString and $endString";
}

sub secToDayAge {
  my $sec=shift;
  return 'Now' if($sec < 60);
  if($sec < 3600) {
    my $nbMin=int($sec/60);
    return "$nbMin min. ago";
  }
  if($sec < 86400) {
    my $nbHours=int($sec/3600);
    return "$nbHours hour".($nbHours > 1 ? 's' : '').' ago';
  }
  my $nbDays=int($sec/86400);
  return "Yesterday" if($nbDays < 2);
  return "$nbDays days ago";
}

sub realLength {
  my $s=shift;
  $s=~s/\d{1,2}(?:,\d{1,2})?//g;
  $s=~s/[]//g;
  return length($s);
}

sub formatList {
  my ($p_list,$maxLength)=@_;
  return '' unless(@{$p_list});
  return '...' if(realLength($p_list->[0]) > $maxLength || ($#{$p_list} > 0 && realLength("$p_list->[0]...") > $maxLength));
  my $result=$p_list->[0];
  for my $i (1..$#{$p_list}) {
    if($i == $#{$p_list}) {
      return "$result..." if(realLength("$result,$p_list->[$i]") > $maxLength);
    }else{
      return "$result..." if(realLength("$result,$p_list->[$i]...") > $maxLength);
    }
    $result.=",$p_list->[$i]";
  }
  return $result;
}

sub rightPadString {
  my ($string,$size)=@_;
  my $length=realLength($string);
  if($length < $size) {
    $string.=' 'x($size-$length);
  }elsif($length > $size) {
    $string=substr($string,0,$size-3);
    $string.='...';
  }
  return $string;
}

sub formatArray {
  my ($p_fields,$p_entries,$title,$maxLength)=@_;
  $title='' unless(defined $title);
  $maxLength=100 unless(defined $maxLength);
  my @fields=@{$p_fields};
  my @entries=@{$p_entries};
  my @rows;
  my $rowLength=0;
  $#rows=$#entries+3;
  for my $i (0..$#rows) {
    $rows[$i]='';
  }
  for my $i (0..$#fields) {
    my $field=$fields[$i];
    my $length=getMaxLength($field,$p_entries);
    $length=$maxLength if($length > $maxLength);
    $rowLength+=$length;
    for my $j (0..$#rows) {
      if($j==0) {
        $rows[0].=rightPadString($field,$length);
      }elsif($j==1) {
        $rows[1].=('-' x $length);
      }elsif($j==$#rows) {
        $rows[$j].=('=' x $length);
      }elsif(exists $entries[$j-2]->{$field} && defined $entries[$j-2]->{$field}) {
        $rows[$j].=rightPadString($entries[$j-2]->{$field},$length);
      }else{
        $rows[$j].=(' ' x $length);
      }
      if($i != $#fields) {
        if($j == $#rows) {
          $rows[$j].="==";
        }else{
          $rows[$j].="  ";
        }
      }
    }
  }
  if($title) {
    $rowLength+=$#fields * 2 if($#fields > 0);
    if(realLength($title) < $rowLength-3) {
      $title="[ $title ]";
      $title=(' ' x int(($rowLength-realLength($title))/2)).$title.(' ' x ceil(($rowLength-realLength($title))/2));
    }elsif(realLength($title) < $rowLength-1) {
      $title="[$title]";
      $title=(' ' x int(($rowLength-realLength($title))/2)).$title.(' ' x ceil(($rowLength-realLength($title))/2));
    }
    unshift(@rows,$title);
  }
  return \@rows;
}

sub getMaxLength {
  my ($field,$p_entries)=@_;
  my $length=realLength($field);
  foreach my $entry (@{$p_entries}) {
    if(exists $entry->{$field} && defined $entry->{$field} && realLength($entry->{$field}) > $length) {
      $length=realLength($entry->{$field});
    }
  }
  return $length;
}

sub formatNumber {
  my $n=shift;
  $n=sprintf("%.1f",$n) if($n=~/^\d+\.\d+$/);
  return $n;
}

sub formatInteger {
  my $n=shift;
  if($n >= 100000000) {
    $n=int($n / 1000000);
    $n.='M.';
  }elsif($n >= 100000) {
    $n=int($n / 1000);
    $n.='K.';
  }
  return $n;
}

sub getCpuSpeed {
  return 0 if($win);
  if(-f "/proc/cpuinfo" && -r "/proc/cpuinfo") {
    my @cpuInfo=`cat /proc/cpuinfo 2>/dev/null`;
    my %cpu;
    foreach my $line (@cpuInfo) {
      if($line =~ /^([\w\s]*\w)\s*:\s*(.*)$/) {
        $cpu{$1}=$2;
      }
    }
    if(defined $cpu{"model name"} && $cpu{"model name"} =~ /(\d+)\+/) {
      return $1;
    }
    if(defined $cpu{"cpu MHz"} && $cpu{"cpu MHz"} =~ /^(\d+)(?:\.\d*)?$/) {
      return $1;
    }
    if(defined $cpu{bogomips} && $cpu{bogomips} =~ /^(\d+)(?:\.\d*)?$/) {
      return $1;
    }
    slog("Unable to parse CPU info from /proc/cpuinfo",2);
    return 0;
  }else{
    slog("Unable to retrieve CPU info from /proc/cpuinfo",2);
    return 0;
  }
}

sub getLocalLanIp {
  return '*' if($win);
  my @ips;

  $ENV{LANG}="C";
  my $ifconfigBin;
  if(-x '/sbin/ifconfig') {
    $ifconfigBin='/sbin/ifconfig';
  }elsif(-x '/bin/ifconfig') {
    $ifconfigBin='/bin/ifconfig';
  }else{
    $ifconfigBin='ifconfig';
  }
  my @ifConfOut=`$ifconfigBin`;
  foreach my $line (@ifConfOut) {
    next unless($line =~ /inet addr:\s*(\d+\.\d+\.\d+\.\d+)\s/);
    push(@ips,$1);
  }
  foreach my $ip (@ips) {
    if($ip =~ /^10\./ || $ip =~ /192\.168\./) {
      slog("Following local LAN IP address detected: $ip",4);
      return $ip;
    }
    if($ip =~ /^172\.(\d+)\./) {
      if($1 > 15 && $1 < 32) {
        slog("Following local LAN IP address detected: $ip",4);
        return $ip;
      }
    }
  }
  slog("No local LAN IP address found",4);
  return "*";
}

sub scheduleQuit {
  my ($reason,$type)=@_;
  $type=1 unless(defined $type);
  $quitScheduled=$type;
  my %quitTypes=(1 => 'shutdown',
                 2 => 'restart');
  my $msg="Bot $quitTypes{$type} scheduled (reason: $reason)";
  broadcastMsg($msg);
  slog($msg,3);
}

sub computeMessageSize {
  my $p_msg=shift;
  my $size=0;
  {
    use bytes;
    foreach my $word (@{$p_msg}) {
      $size+=length($word)+1;
    }
  }
  return $size;
}

sub checkLastSentMessages {
  my $sent=0;
  foreach my $timestamp (keys %lastSentMessages) {
    if(time - $timestamp > $conf{sendRecordPeriod}) {
      delete $lastSentMessages{$timestamp};
    }else{
      foreach my $msgSize (@{$lastSentMessages{$timestamp}}) {
        $sent+=$msgSize;
      }
    }
  }
  return $sent;
}

sub queueLobbyCommand {
  my @params=@_;
  if($params[0]->[0] =~ /SAYPRIVATE/) {
    push(@lowPriorityMessageQueue,\@params);
  }elsif(@messageQueue) {
    push(@messageQueue,\@params);
  }else{
    my $alreadySent=checkLastSentMessages();
    my $toBeSent=computeMessageSize($params[0]);
    if($alreadySent+$toBeSent+5 >= $conf{maxBytesSent}) {
      slog("Output flood protection: queueing message(s)",2);
      push(@messageQueue,\@params);
    }else{
      sendLobbyCommand(\@params,$toBeSent);
    }
  }
}

sub sendLobbyCommand {
  my ($p_params,$size)=@_;
  $size=computeMessageSize($p_params->[0]) unless(defined $size);
  my $timestamp=time;
  $lastSentMessages{$timestamp}=[] unless(exists $lastSentMessages{$timestamp});
  push(@{$lastSentMessages{$timestamp}},$size);
  if(! $lobby->sendCommand(@{$p_params})) {
    $lobbyBrokenConnection=1 if($lobbyState > 0);
  }
}

sub checkQueuedLobbyCommands {
  return unless($lobbyState > 1 && (@messageQueue || @lowPriorityMessageQueue));
  my $alreadySent=checkLastSentMessages();
  while(@messageQueue) {
    my $toBeSent=computeMessageSize($messageQueue[0]->[0]);
    last if($alreadySent+$toBeSent+5 >= $conf{maxBytesSent});
    my $p_command=shift(@messageQueue);
    sendLobbyCommand($p_command,$toBeSent);
    $alreadySent+=$toBeSent;
  }
  my $nbMsgSentInLoop=0;
  while(@lowPriorityMessageQueue && $nbMsgSentInLoop < 100) {
    my $toBeSent=computeMessageSize($lowPriorityMessageQueue[0]->[0]);
    last if($alreadySent+$toBeSent+5 >= $conf{maxLowPrioBytesSent});
    my $p_command=shift(@lowPriorityMessageQueue);
    sendLobbyCommand($p_command,$toBeSent);
    $alreadySent+=$toBeSent;
    $nbMsgSentInLoop++;
  }
}

sub answer {
  my $msg=shift;
  &{$p_answerFunction}($msg);
}

sub broadcastMsg {
  my $msg=shift;
  my @broadcastChans=split(/;/,$conf{broadcastChannels});
  foreach my $chan (@broadcastChans) {
    $chan=$1 if($chan =~ /^([^\s]+)\s/);
    sayChan($chan,$msg);
  }
}

sub splitMsg {
  my ($longMsg,$maxSize)=@_;
  my @messages=($longMsg =~ /.{1,$maxSize}/gs);
  return \@messages;
}

sub sayPrivate {
  my ($user,$msg)=@_;
  my $p_messages=splitMsg($msg,$conf{maxChatMessageLength}-12-length($user));
  foreach my $mes (@{$p_messages}) {
    queueLobbyCommand(["SAYPRIVATE",$user,$mes]);
    logMsg("pv_$user","<$conf{lobbyLogin}> $mes") if($conf{logPvChat});
  }
}

sub sayChan {
  my ($chan,$msg)=@_;
  return unless($lobbyState >= 4 && (exists $lobby->{channels}->{$chan}));
  my $p_messages=splitMsg($msg,$conf{maxChatMessageLength}-9-length($chan));
  foreach my $mes (@{$p_messages}) {
    queueLobbyCommand(["SAYEX",$chan,"* $mes"]);
  }
}

sub getCommandLevels {
  my ($source,$user,$cmd)=@_;
  return $botConf->getCommandLevels($cmd,$source,'outside','stopped');
}

sub getUserAccessLevel {
  my $user=shift;
  my $p_userData;
  if(! exists $lobby->{users}->{$user}) {
    return 0;
  }else{
    $p_userData=$lobby->{users}->{$user};
  }
  return $botConf->getUserAccessLevel($user,$p_userData,! $lanMode);
}

sub handleRequest {
  my ($source,$user,$command,$floodCheck)=@_;
  $floodCheck=1 unless(defined $floodCheck);
  
  return if($floodCheck && checkCmdFlood($user));
  
  my %answerFunctions = ( pv => sub { sayPrivate($user,$_[0]) },
                          chan => sub { sayChan($masterChannel,$_[0]) } );
  $p_answerFunction=$answerFunctions{$source};
  
  my @cmd=grep {$_ ne ''} (split(/ /,$command));
  my $lcCmd=lc($cmd[0]);

  if(exists $botConf->{commands}->{$lcCmd}) {
    slog("Start of \"$lcCmd\" command processing",5);
    
    my $p_levels=getCommandLevels($source,$user,$lcCmd);
    
    my $level=getUserAccessLevel($user);

    if(defined $p_levels->{directLevel} && $p_levels->{directLevel} ne '' && $level >= $p_levels->{directLevel}) {
      executeCommand($source,$user,\@cmd);
    }else{
      answer("$user, you are not allowed to call command \"$cmd[0]\" in current context.");
    }

    slog("End of \"$lcCmd\" command processing",5);
  }else{
    answer("Invalid command \"$cmd[0]\"") unless($source eq "chan");
  }
}

sub executeCommand {
  my ($source,$user,$p_cmd)=@_;

  my %answerFunctions = ( pv => sub { sayPrivate($user,$_[0]) },
                          chan => sub { sayChan($masterChannel,$_[0]) } );
  $p_answerFunction=$answerFunctions{$source};

  my @cmd=@{$p_cmd};
  my $command=lc(shift(@cmd));

  if(exists $lobbyHandlers{$command}) {
    return &{$lobbyHandlers{$command}}($source,$user,\@cmd);
  }else{
    answer("Invalid command \"$command\"");
    return 0;
  }

}

sub invalidSyntax {
  my ($user,$cmd,$reason)=@_;
  $reason='' unless(defined $reason);
  $reason=" (".$reason.")" if($reason);
  answer("Invalid $cmd command usage$reason. $user, please refer to help sent in private message.");
  executeCommand("pv",$user,["help",$cmd]);
}
  

sub checkTimedEvents {
}

sub checkCmdFlood {
  my $user=shift;

  my $timestamp=time;
  $lastCmds{$user}={} unless(exists $lastCmds{$user});
  $lastCmds{$user}->{$timestamp}=0 unless(exists $lastCmds{$user}->{$timestamp});
  $lastCmds{$user}->{$timestamp}++;
  
  return 0 if(getUserAccessLevel($user) >= $conf{floodImmuneLevel});

  if(exists $ignoredUsers{$user}) {
    if(time > $ignoredUsers{$user}) {
      delete $ignoredUsers{$user};
    }else{
      return 1;
    }
  }

  my @autoIgnoreData=split(/;/,$conf{cmdFloodAutoIgnore});

  my $received=0;
  foreach my $timestamp (keys %{$lastCmds{$user}}) {
    if(time - $timestamp > $autoIgnoreData[1]) {
      delete $lastCmds{$user}->{$timestamp};
    }else{
      $received+=$lastCmds{$user}->{$timestamp};
    }
  }

  if($autoIgnoreData[0] && $received >= $autoIgnoreData[0]) {
    broadcastMsg("Ignoring $user for $autoIgnoreData[2] minute(s) (command flood protection)");
    $ignoredUsers{$user}=time+($autoIgnoreData[2] * 60);
    return 1;
  }
  
  return 0;
}

sub logMsg {
  my ($file,$msg)=@_;
  if(! -d $conf{logDir}."/chat") {
    if(! mkdir($conf{logDir}."/chat")) {
      slog("Unable to create directory \"$conf{logDir}/chat\"",1);
      return;
    }
  }
  if(! open(CHAT,">>$conf{logDir}/chat/$file.log")) {
    slog("Unable to log chat message into file \"$conf{logDir}/chat/$file.log\"",1);
    return;
  }
  my $dateTime=localtime();
  print CHAT "[$dateTime] $msg\n";
  close(CHAT);
}

sub initUserIrcColors {
  my $user=shift;
  return @noIrcStyle;
}

# SpringLobbyBot commands handlers #####################################################

sub hHelp {
  my ($source,$user,$p_params)=@_;
  my ($cmd)=@{$p_params};

  my ($p_C,$B)=initUserIrcColors($user);
  my %C=%{$p_C};
  
  if(defined $cmd) {
    my $helpCommand=lc($cmd);
    $helpCommand=$1 if($helpCommand =~ /^!(.+)$/);
    if($helpCommand !~ /^\w+$/) {
      invalidSyntax($user,"help");
      return 0;
    }
    if(exists $botConf->{help}->{$helpCommand}) {

      my $p_help=$botConf->{help}->{$helpCommand};

      sayPrivate($user,"$B********** Help for command $C{12}$cmd$C{1} **********");
      sayPrivate($user,"$B$C{10}Syntax:");
      my $helpLine=$p_help->[0];
      $helpLine="$C{12}$1$C{5}$2$C{1}$3" if($helpLine =~ /^(!\w+)(.*)( - .*)$/);
      sayPrivate($user,'  '.$helpLine);
      sayPrivate($user,"$B$C{10}Example(s):") if($#{$p_help} > 0);
      for my $i (1..$#{$p_help}) {
        $helpLine=$p_help->[$i];
        $helpLine="\"$C{3}$1$C{1}\"$2" if($helpLine =~ /^\"([^\"]+)\"(.+)$/);
        sayPrivate($user,'  '.$helpLine);
      }

    }else{
      sayPrivate($user,"\"$C{12}$cmd$C{1}\" is not a valid command or setting.");
    }
  }else{

    my $level=getUserAccessLevel($user);
    my $p_helpForUser=$botConf->getHelpForLevel($level);

    sayPrivate($user,"$B********** Available commands for your access level **********");
    foreach my $i (0..$#{$p_helpForUser->{direct}}) {
      $p_helpForUser->{direct}->[$i]="$C{3}$1$C{5}$2$C{1}$3" if($p_helpForUser->{direct}->[$i] =~ /^(!\w+)(.*)( - .*)$/);
      sayPrivate($user,$p_helpForUser->{direct}->[$i]);
    }
  }

}

sub hHelpAll {
  my (undef,$user,undef)=@_;

  my ($p_C,$B)=initUserIrcColors($user);
  my %C=%{$p_C};
  
  my $p_help=$botConf->{help};

  sayPrivate($user,"$B********** SpringLobbyBot commands **********");
  for my $command (sort (keys %{$p_help})) {
    next unless($command);
    my $helpLine=$p_help->{$command}->[0];
    $helpLine="$C{3}$1$C{5}$2$C{1}$3" if($helpLine =~ /^(!\w+)(.*)( - .*)$/);
    sayPrivate($user,$helpLine);
  }
}

sub hQuit {
  my ($source,$user)=@_;
  my %sourceNames = ( pv => 'private',
                      chan => "channel #$masterChannel" );
  scheduleQuit("requested by $user in $sourceNames{$source}");
}

sub hReloadConf {
  my ($source,$user)=@_;

  my $newSpringLobbyBot=SpringLobbyBotConf->new($confFile,$sLog);
  if(! $newSpringLobbyBot) {
    answer("Unable to reload SpringLobbyBot configuration");
    return 0;
  }

  $botConf=$newSpringLobbyBot;
  %conf=%{$botConf->{conf}};

  answer('SpringLobbyBot configuration reloaded');
}

sub hRestart {
  my ($source,$user)=@_;
   my %sourceNames = ( pv => "private",
                       chan => "channel #$conf{masterChannel}");
  scheduleQuit("requested by $user in $sourceNames{$source}",2);
}

sub hVersion {
  my (undef,$user,undef)=@_;

  my ($p_C,$B)=initUserIrcColors($user);
  my %C=%{$p_C};
  
  sayPrivate($user,"$C{12}$conf{lobbyLogin}$C{1} is running ${B}$C{5}SpringLobbyBot $C{10}v$SpringLobbyBotVer$B$C{1}, with following components:");
  sayPrivate($user,"- $C{5}Perl$C{10} $^V");
  my %components = (SpringLobbyInterface => $lobby,
                    SpringLobbyBotConf => $botConf,
                    SimpleLog => $sLog);
  foreach my $module (keys %components) {
    my $ver=$components{$module}->getVersion();
    sayPrivate($user,"- $C{5}$module$C{10} v$ver");
  }

}

# Lobby interface callbacks ###################################################

sub cbLobbyConnect {
  $lobbyState=2;
  $lobbyBrokenConnection=0;
  $lanMode=$_[4];

  $lobby->addCallbacks({CHANNELTOPIC => \&cbChannelTopic,
                        LOGININFOEND => \&cbLoginInfoEnd,
                        JOIN => \&cbJoin,
                        JOINFAILED => \&cbJoinFailed,
                        SAID => \&cbSaid,
                        CHANNELMESSAGE => \&cbChannelMessage,
                        SAIDEX => \&cbSaidEx,
                        SAIDPRIVATE => \&cbSaidPrivate,
                        BROADCAST => \&cbBroadcast,
                        JOINED => \&cbJoined,
                        LEFT => \&cbLeft});

  my $localLanIp=$conf{localLanIp};
  $localLanIp=getLocalLanIp() unless($localLanIp);
  queueLobbyCommand(["LOGIN",$conf{lobbyLogin},$lobby->marshallPasswd($conf{lobbyPassword}),getCpuSpeed(),$localLanIp,"SpringLobbyBot v$SpringLobbyBotVer",0,'l t b sp cl'],
                    {ACCEPTED => \&cbLoginAccepted,
                     DENIED => \&cbLoginDenied,
                     AGREEMENTEND => \&cbAgreementEnd},
                    \&cbLoginTimeout);
}

sub cbBroadcast {
  my (undef,$msg)=@_;
  print "Lobby broadcast message: $msg\n";
  slog("Lobby broadcast message: $msg",3);
}

sub cbRedirect {
  my (undef,$ip,$port)=@_;
  $ip='' unless(defined $ip);
  if($conf{lobbyFollowRedirect}) {
    if($ip =~ /^(\d+)\.(\d+)\.(\d+)\.(\d+)$/ && $1<256 && $2<256 && $3<256 && $4<256) {
      $port=$conf{lobbyPort} unless(defined $port);
      if($port !~ /^\d+$/) {
        slog("Invalid port \"$port\" received in REDIRECT command, ignoring redirection",1);
        return;
      }
    }else{
      slog("Invalid IP address \"$ip\" received in REDIRECT command, ignoring redirection",1);
      return;
    }
    %pendingRedirect=(ip => $ip, port => $port);
  }else{
    slog("Ignoring redirection request to address $ip",2);
  }
}

sub cbLobbyDisconnect {
  slog("Disconnected from lobby server (connection reset by peer)",2);
  $lobbyState=0;
  foreach my $joinedChan (keys %{$lobby->{channels}}) {
    logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
  }
  $lobby->disconnect();
}

sub cbConnectTimeout {
  $lobbyState=0;
  slog("Timeout while connecting to lobby server ($conf{lobbyHost}:$conf{lobbyPort})",2);
}

sub cbLoginAccepted {
  $lobbyState=3;
  slog("Logged on lobby server",4);
  $triedGhostWorkaround=0;
}

sub cbLoginInfoEnd {
  $lobbyState=4;
  queueLobbyCommand(["JOIN",$conf{masterChannel}]) if($conf{masterChannel} ne '');
  my %chansToJoin;
  if($conf{broadcastChannels}) {
    my @broadcastChans=split(/;/,$conf{broadcastChannels});
    foreach my $chan (@broadcastChans) {
      $chansToJoin{$chan}=1;
    }
  }
  foreach my $chan (keys %chansToJoin) {
    next if($chan eq $conf{masterChannel});
    queueLobbyCommand(["JOIN",$chan]);
  }
  if(exists $lobby->{users}->{$conf{lobbyLogin}} && ! $lobby->{users}->{$conf{lobbyLogin}}->{status}->{bot}) {
    slog('The lobby account currently used by SpringLobbyBot is not tagged as bot. It is recommended to ask a lobby administrator for bot flag on accounts used by SpringLobbyBot',2);
  }
}

sub cbLoginDenied {
  my (undef,$reason)=@_;
  slog("Login denied on lobby server ($reason)",1);
  if(($reason !~ /^Already logged in/ && $reason !~ /^This account has already logged in/) || $triedGhostWorkaround > 2) {
    scheduleQuit("loggin denied on lobby server");
  }
  if($reason =~ /^Already logged in/) {
    $triedGhostWorkaround++;
  }else{
    $triedGhostWorkaround=0;
  }
  $lobbyState=0;
  $lobby->disconnect();
}

sub cbAgreementEnd {
  slog('Spring Lobby agreement has not been accepted for this account yet, please login with a Spring lobby client and accept the agreement',1);
  scheduleQuit('spring Lobby agreement not accepted yet for this account');
  $lobbyState=0;
  $lobby->disconnect();
}

sub cbLoginTimeout {
  slog("Unable to log on lobby server (timeout)",2);
  $lobbyState=0;
  foreach my $joinedChan (keys %{$lobby->{channels}}) {
    logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
  }
  $lobby->disconnect();
}

sub cbJoin {
  my (undef,$channel)=@_;
  slog("Channel $channel joined",4);
  logMsg("channel_$channel","=== $conf{lobbyLogin} joined ===") if($conf{logChanJoinLeave});
}

sub cbJoinFailed {
  my (undef,$channel,$reason)=@_;
  slog("Unable to join channel $channel ($reason)",2);
}

sub cbJoined {
  my (undef,$chan,$user)=@_;
  logMsg("channel_$chan","=== $user joined ===") if($conf{logChanJoinLeave});
}

sub cbLeft {
  my (undef,$chan,$user,$reason)=@_;
  my $reasonString ='';
  $reasonString=" ($reason)" if(defined $reason && $reason ne '');
  logMsg("channel_$chan","=== $user left$reasonString ===") if($conf{logChanJoinLeave});
}

sub cbSaid {
  my (undef,$chan,$user,$msg)=@_;
  logMsg("channel_$chan","<$user> $msg") if($conf{logChanChat});
  if($chan eq $masterChannel && $msg =~ /^!(\w.*)$/) {
    handleRequest("chan",$user,$1);
  }
}

sub cbChannelMessage {
  my (undef,$chan,$msg)=@_;
  logMsg("channel_$chan","* Channel message: $msg") if($conf{logChanChat});
}

sub cbSaidEx {
  my (undef,$chan,$user,$msg)=@_;
  logMsg("channel_$chan","* $user $msg") if($conf{logChanChat});
}

sub cbSaidPrivate {
  my (undef,$user,$msg)=@_;
  logMsg("pv_$user","<$user> $msg") if($conf{logPvChat});
  if($msg =~ /^!(\w.*)$/) {
    handleRequest("pv",$user,$1);
  }
}

sub cbChannelTopic {
  my (undef,$chan,$user,$topic)=@_;
  if($conf{logChanChat}) {
    if(defined $topic && $topic ne '') {
      logMsg("channel_$chan","* Topic is '$topic' (set by $user)");
    }else{
      logMsg("channel_$chan","* No topic is set");
    }
  }
}

# Main ########################################################################

slog("Initializing SpringLobbyBot",3);

while($running) {

  if(! $lobbyState && ! $quitScheduled) {
    if($timestamps{connectAttempt} != 0 && $conf{lobbyReconnectDelay} == 0) {
      scheduleQuit('disconnected from lobby server, no reconnection delay configured');
    }else{
      if(time-$timestamps{connectAttempt} > $conf{lobbyReconnectDelay}) {
        $timestamps{connectAttempt}=time;
        $lobbyState=1;
        if(defined $lSock) {
          my @newSockets=();
          foreach my $sock (@sockets) {
            push(@newSockets,$sock) unless($sock == $lSock);
          }
          @sockets=@newSockets;
        }
        $lobby->addCallbacks({REDIRECT => \&cbRedirect});
        $lSock = $lobby->connect(\&cbLobbyDisconnect,{TASSERVER => \&cbLobbyConnect},\&cbConnectTimeout);
        if($lSock) {
          push(@sockets,$lSock);
        }else{
          $lobby->removeCallbacks(['REDIRECT']);
          $lobbyState=0;
          slog("Connection to lobby server failed",1);
        }
      }
    }
  }

  checkQueuedLobbyCommands();

  checkTimedEvents();

  my @pendingSockets=IO::Select->new(@sockets)->can_read(1);

  foreach my $pendingSock (@pendingSockets) {
    if($pendingSock == $lSock) {
      $lobby->receiveCommand();
    }else{
      scheduleQuit("received data on unknown socket");
    }
  }

  if( $lobbyState > 0 && ( (time - $timestamps{connectAttempt} > 30 && time - $lobby->{lastRcvTs} > 60) || $lobbyBrokenConnection ) ) {
    if($lobbyBrokenConnection) {
      $lobbyBrokenConnection=0;
      slog("Disconnecting from lobby server (broken connection detected)",2);
    }else{
      slog("Disconnected from lobby server (timeout)",2);
    }
    $lobbyState=0;
    foreach my $joinedChan (keys %{$lobby->{channels}}) {
      logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
    }
    $lobby->disconnect();
  }

  if($lobbyState > 1 && ( ( time - $timestamps{ping} > 5 && time - $lobby->{lastSndTs} > 28)
                          || ( time - $timestamps{ping} > 28 && time - $lobby->{lastRcvTs} > 28) ) ) {
    sendLobbyCommand([['PING']],5);
    $timestamps{ping}=time;
  }

  if(%pendingRedirect) {
    my ($ip,$port)=($pendingRedirect{ip},$pendingRedirect{port});
    %pendingRedirect=();
    slog("Following redirection to $ip:$port",3);
    $lobbyState=0;
    foreach my $joinedChan (keys %{$lobby->{channels}}) {
      logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
    }
    $lobby->disconnect();
    $conf{lobbyHost}=$ip;
    $conf{lobbyPort}=$port;
    $lobby = SpringLobbyInterface->new(serverHost => $conf{lobbyHost},
                                       serverPort => $conf{lobbyPort},
                                       simpleLog => $lobbySimpleLog,
                                       warnForUnhandledMessages => 0);
    $timestamps{connectAttempt}=0;
  }

  if($quitScheduled) {
    slog("No pending process, exiting",3);
    $running=0;
  }
}

if($lobbyState) {
  foreach my $joinedChan (keys %{$lobby->{channels}}) {
    logMsg("channel_$joinedChan","=== $conf{lobbyLogin} left ===") if($conf{logChanJoinLeave});
  }
  $lobbyState=0;
  if($quitScheduled == 2) {
    sendLobbyCommand([['EXIT','SpringLobbyBot restarting']]);
  }else{
    sendLobbyCommand([['EXIT','SpringLobbyBot shutting down']]);
  }
  $lobby->disconnect();
}
if($quitScheduled == 2) {
  exec($0,$confFile) || forkedError("Unable to restart SpringLobbyBot",0);
}

exit 0;
