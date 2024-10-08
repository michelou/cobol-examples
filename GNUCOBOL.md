# <span id="top">GnuCOBOL</span> <span style="font-size:90%;">[↩](./README.md#top)</span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 4px 0 0;min-width:100px;"><a href="https://gnucobol.sourceforge.io/" rel="external"><img style="border:0;" src="docs/images/gnucobol.png" width="100" alt="GnuCOBOL"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This page presents usage information about <a href="https://gnucobol.sourceforge.io/" rel="external">GnuCOBOL </a> on a Windows machine.</td>
  </tr>
</table> 


## <span id="env">GnuCOBOL environment</span>

Configuration ot the <a href="https://gnucobol.sourceforge.io/" rel="external">GnuCOBOL</a> compiler occurs in two ways :
<ul>
<li>through command line options (e.g. option <code>--free</code>) and
<li>through the execution environment (see variables below).
</ul>
<pre style="font-size:80%;">
<a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/set_1" rel="external"><b>set</b></a> COB_CC=%COB_HOME%\mingw64\bin\gcc.exe
<b>set</b> COB_CFLAGS=-pipe -I%%COB_HOME%\include -Wno-unused -fsigned-char -Wno-pointer-sign
<b>set</b> COB_LIBS=-L "%COB_HOME%\lib" -lcob
<b>set</b> COB_CONFIG_DIR=c%COB_HOME%\config
<b>set</b> COB_COPY_DIR=%COB_HOME%\copy
</pre>

> **Note**: The environment variable `COB_CONFIG_DIR` must be set for compiler option `-std=cobol2002` in order to find the corresponding configuration file.<br/>
> GnuCOBOL 3.2 provides the following configuration files :
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/echo" rel="external">echo</a> %COB_CONFIG_DIR%</b>
> C:\opt\GnuCOBOL\config
> &nbsp;
> <b>&gt; <a href="https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/dir" rel="external">dir</a> /s /b C:\opt\GnuCOBOL\config\*.conf</b>
> C:\opt\GnuCOBOL\config\acu-strict.conf
> C:\opt\GnuCOBOL\config\acu.conf
> C:\opt\GnuCOBOL\config\bs2000-strict.conf
> C:\opt\GnuCOBOL\config\bs2000.conf
> C:\opt\GnuCOBOL\config\cobol2002.conf
> C:\opt\GnuCOBOL\config\cobol2014.conf
> C:\opt\GnuCOBOL\config\cobol85.conf
> C:\opt\GnuCOBOL\config\default.conf
> C:\opt\GnuCOBOL\config\gcos-strict.conf
> C:\opt\GnuCOBOL\config\gcos.conf
> C:\opt\GnuCOBOL\config\ibm-strict.conf
> C:\opt\GnuCOBOL\config\ibm.conf
> C:\opt\GnuCOBOL\config\mf-strict.conf
> C:\opt\GnuCOBOL\config\mf.conf
> C:\opt\GnuCOBOL\config\mvs-strict.conf
> C:\opt\GnuCOBOL\config\mvs.conf
> C:\opt\GnuCOBOL\config\realia-strict.conf
> C:\opt\GnuCOBOL\config\realia.conf
> C:\opt\GnuCOBOL\config\rm-strict.conf
> C:\opt\GnuCOBOL\config\rm.conf
> C:\opt\GnuCOBOL\config\xopen.conf
> </pre>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/October 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->
[gnucobol]: https://gnucobol.sourceforge.io/
