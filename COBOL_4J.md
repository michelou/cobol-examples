# <span id="top">COBOL 4J</span> <span style="font-size:90%;">[↩](./README.md#top)</span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 4px 0 0;min-width:100px;"><a href=" rel="external"><img style="border:0;" src="docs/images/opensource-cobol.png" width="100" alt="COBOL 4J"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">.
  </td>
  </tr>
</table>

## <span id="env">Environment Setup</span>

Invocation of the the [COBOL 4J][cobol_4j] compiler requires that :
- Environment variable `PATH` contains the path to executable `java.exe`.
- Environment variable `CLASSPATH` contains the path to library `libcobj.jar`.

For instance we invoke the [COBOL 4J][cobol_4j] compiler as follows in our batch files :
<pre style="font-size:80%;">
[...]
<b>set</b> "__PATH=%PATH%"
<b>set</b> "PATH=%PATH%;%JAVA_HOME%\bin"
<b>set</b> "__CLASSPATH=%CLASSPATH%"
<b>if defined</b> CLASSPATH ( <b>set</b> "CLASSPATH=%CLASSPATH%;%COBJ_HOME%\lib\opensourcecobol4j\libcobj.jar"
) <b>else</b> ( <b>set</b> "CLASSPATH=%COBJ_HOME%\lib\opensourcecobol4j\libcobj.jar"
)
<b>call</b> "%_COBJ_CMD%" %__COBJ_OPTS% %__SOURCE_FILES:\=/%
<b>set</b> "PATH=%__PATH%"
<b>set</b> "CLASSPATH=%__CLASSPATH%"
[...]
</pre>
where the defined variables :
- `COBJ_HOME` equals `C:\opt\cobj` (our [COBOL 4J][cobol_4j] installation directory).
- `_COBJ_CMD` equals `%COBJ_HOME%\bin\cobj.exe`.

> :mag_right: We distinguish two kinds of variabls in our batch files :
> - Variables starting with a letter are defined either in [`setenv.bat`](../setenv.bat) (eg. `COBJ_HOME`) or by the Windows environment (eg. `PATH`).
> - Variables starting with one (or more) `_` character(s) are always local to the build file where they appear.

<!--
%JAVA_HOME%\bin\jar tf %COBJ_HOME%\lib\opensourcecobol4j\libcobj.jar|awk '/\/$/{if ($0 !~ /^META/){s=$0;gsub(/\//,"",s);if (length($0)==length(s)+3){print $0}}}
-->

Version 1.022 of library `libcobj.jar` has the following dependencies (see also Gradle build file [`build.gradle.kts`](https://github.com/opensourcecobol/opensourcecobol4j/blob/develop/libcobj/app/build.gradle.kts#L29)) :

| Project | MVN&nbsp;repository | Version |
|:-------|:--------------------|:--------|
| [Guava](https://github.com/google/guava#guava-google-core-libraries-for-java) | [com.google.common](https://mvnrepository.com/artifact/com.google.guava/guava) | [31.1-jre](https://mvnrepository.com/artifact/com.google.guava/guava/31.1-jre) |
| [Guava](https://github.com/google/guava#guava-google-core-libraries-for-java) | [com.google.thirdparty](https://mvnrepository.com/artifact/com.google.guava/guava) | [31.1-jre](https://mvnrepository.com/artifact/com.google.guava/guava/31.1-jre) |
| [SQLite JDBC](https://github.com/xerial/sqlite-jdbc#sqlite-jdbc-driver) | [org.sqlite](https://mvnrepository.com/artifact/org.xerial/sqlite-jdbc) | [3.30.1](https://mvnrepository.com/artifact/org.xerial/sqlite-jdbc/3.30.1)C |
| [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/) | [commons-cli](https://mvnrepository.com/artifact/commons-cli/commons-cli) | [1.6.0](https://mvnrepository.com/artifact/commons-cli/commons-cli/1.6.0) |
| [Check Framework](https://checkerframework.org/) | [org.checkerframework](https://mvnrepository.com/artifact/org.checkerframework/checker-qual) | [3.42.0](https://mvnrepository.com/artifact/org.checkerframework/checker-qual/3.42.0) |
| [J2ObjC Annotations](https://mvnrepository.com/artifact/com.google.j2objc/j2objc-annotations) | [j2obj-annotations](https://mvnrepository.com/artifact/com.google.j2objc/j2objc-annotations) | [3.0.0](https://mvnrepository.com/artifact/com.google.j2objc/j2objc-annotations/3.0.0) |
| [Error Prone Annotations](https://github.com/google/error-prone#error-prone) | [com.google.errorprone](https://mvnrepository.com/artifact/com.google.errorprone/error_prone_annotations) | [2.26.1](https://mvnrepository.com/artifact/com.google.errorprone/error_prone_annotations/2.26.1) |
<!--
javax/annotation/concurrent/
javax/annotation/meta/
com/google/errorprone/
-->

***

*[mics](https://lampwww.epfl.ch/~michelou/)/June 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->
[cobol_4j]: https://github.com/opensourcecobol/opensourcecobol4j
