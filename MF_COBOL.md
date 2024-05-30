# <span id="top">Visual COBOL</span> <span style="font-size:90%;">[↩](./README.md#top)</span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 4px 0 0;min-width:100px;"><a href=" rel="external"><img style="border:0;" src="docs/images/Micro-Focus.png" width="100" alt="Visual COBOL"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">.
  </td>
  </tr>
</table>

## <span id="env">Environment Setup</span>

Setting up the Micro Focus envrionment :

<pre style="font-size:80%;">
<b>&gt; echo %COBDIR%</b>
C:\Program Files (x86)\Micro Focus\Visual COBOL
&nbsp;
<b>&gt; "%COBDIR%\bin64\cblpromp.exe"</b>
@SET COBDIR=C:\Program Files (x86)\Micro Focus\Visual COBOL\;%COBDIR%
@SET PATH=C:\Program Files (x86)\Micro Focus\Visual COBOL\bin64\;C:\Program Files (x86)\Micro Focus\Visual COBOL\binn64\;C:\Program Files (x86)\Micro Focus\Visual COBOL\bin\;C:\Program Files (x86)\Micro Focus\Visual COBOL\AdoptOpenJDK\bin;C:\Program Files (x86)\Micro Focus\Visual COBOL\AdoptRedis\;%PATH% @SET LIB=C:\Program Files (x86)\Micro Focus\Visual COBOL\lib64\;%LIB%
@SET COBCPY=%COBCPY%;C:\Program Files (x86)\Micro Focus\Visual COBOL\cpylib\;C:\Program Files (x86)\Micro Focus\Visual COBOL\cpylib\basecl
@SET MFTRACE_ANNOTATIONS=C:\Program Files (x86)\Micro Focus\Visual COBOL\etc\mftrace\annotations
@SET MFTRACE_LOGS=C:\ProgramData\Micro Focus\Visual COBOL\9.0\mftrace\logs
@SET INCLUDE=C:\Program Files (x86)\Micro Focus\Visual COBOL\include;%INCLUDE%
@SET JAVA_HOME=C:\Program Files (x86)\Micro Focus\Visual COBOL\AdoptOpenJDK
@SET CLASSPATH=C:\Program Files (x86)\Micro Focus\Visual COBOL\bin\mfcobol.jar;C:\Program Files (x86)\Micro Focus\Visual COBOL\bin\mfcobolrts.jar;C:\Program Files (x86)\Micro Focus\Visual COBOL\bin\mfsqljvm.jar;C:\Program Files (x86)\Micro Focus\Visual COBOL\bin\mfunit.jar;C:\Program Files (x86)\Micro Focus\Visual COBOL\bin\mfidmr.jar;C:\Program Files (x86)\Micro Focus\Visual COBOL\bin64\mfle370.jar;%CLASSPATH%
@SET MFDBFH_SCRIPT_DIR=C:\Program Files (x86)\Micro Focus\Visual COBOL\etc\mfdbfh\scripts
@SET TXDIR=C:\Program Files (x86)\Micro Focus\Visual COBOL\
@SET Path=C:\Program Files (x86)\Micro Focus\Visual COBOL\Microsoft\VC\Tools\MSVC\14.34.31933\bin\Hostx64\x64;C:\Program Files (x86)\Micro Focus\Visual COBOL\Microsoft\SDK\10\Bin\10.0.19041.0\x64;%PATH%
@SET LIB=C:\Program Files (x86)\Micro Focus\Visual COBOL\Microsoft\VC\Tools\MSVC\14.34.31933\lib\x64;C:\Program Files (x86)\Micro Focus\Visual COBOL\Microsoft\SDK\10\Lib\10.0.19041.0\um\x64;C:\Program Files (x86)\Micro Focus\Visual COBOL\Microsoft\SDK\10\Lib\10.0.19041.0\ucrt\x64;%LIB%
@SET COBREG_64_PARSED=True
</pre>

<pre style="font-size:80%;">
<b>&gt; "%COBDIR%\AdoptOpenJDK\bin\java.exe" -version</b>
openjdk version "17.0.4.1" 2022-08-12
OpenJDK Runtime Environment Temurin-17.0.4.1+1 (build 17.0.4.1+1)
OpenJDK 64-Bit Server VM Temurin-17.0.4.1+1 (build 17.0.4.1+1, mixed mode, sharing)
</pre>

<pre style="font-size:80%;">
<b>&gt; "%COBDIR%\bin64\cblms.exe"</b>
Micro Focus COBOL - Configuration Utility for the Microsoft Build Tools & SDK
9.0.0.49 (C) Copyright 1984-2023 Micro Focus or one of its affiliates.
&nbsp;
options:
&nbsp;
-U              Update to all latest versions installed in default folders
-U&lt;p&gt;           Update to latest version installed in default folder
-U&lt;p>:&lt;version> Update to version installed in default folder
-U&lt;p>:&lt;path>    Update to latest version installed in given folder
-U&lt;p>:&lt;path>?&lt;version> Update to version installed in given folder
-L              List all versions installed in default folder
-L&lt;p>           List versions installed in default folders
-L&lt;p>:&lt;path>    List versions installed in given folder
-Q              Display current selected versions
-Q&lt;p&gt;           Display current selected version
-R              Clear all version information
-F:&lt;file&gt;       Redirect all output to given file
-64             Update for 64-bit only
-H              Display usage help
&nbsp;
&nbsp;
For each command:
        <p>       is S or SDK for Windows SDK
                     B or BT  for Microsoft Build Tools
        <version> is n.n.n.n for Windows SDK
                     n.n.n   for Build Tools
                     n       for list Id
        <path>    is installation folder
</pre>

Lists all versions of the Microsoft Build Tools and SDK packages that are located in the
default folders.

<pre style="font-size:80%;">
<b>&gt; "C:\Program Files (x86)\Micro Focus\Visual COBOL\bin64\cblms.exe" -L</b>
Micro Focus COBOL - Configuration Utility for the Microsoft Build Tools & SDK
9.0.0.49 (C) Copyright 1984-2023 Micro Focus or one of its affiliates.



Windows SDK

Id  Version       Location

0] 10.0.18362.0  c:\Program Files (x86)\Windows Kits\10
1] 10.0.19041.0  c:\Program Files (x86)\Windows Kits\10
2] 10.0.22000.0  c:\Program Files (x86)\Windows Kits\10


Microsoft Build Tools

Id  Version       Location

0] 14.39.33519 c:\Program Files\Microsoft Visual Studio\2022\Community
1] 14.29.30133 c:\Program Files (x86)\Microsoft Visual Studio\2019\Community
</pre>

Displays the versions currently in use by the COBOL environment.

<pre style="font-size:80%;">
<b>&gt; "C:\Program Files (x86)\Micro Focus\Visual COBOL\bin64\cblms.exe" -Q</b>
Micro Focus COBOL - Configuration Utility for the Microsoft Build Tools & SDK
9.0.0.49 (C) Copyright 1984-2023 Micro Focus or one of its affiliates.


Windows SDK
 location = C:\Program Files (x86)\Micro Focus\Visual COBOL\Microsoft\SDK\10
 version  = 10.0.19041.0

Microsoft Build Tools
 location = C:\Program Files (x86)\Micro Focus\Visual COBOL\Microsoft
 version  = 14.34.31933
</pre>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/June 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->
