# file-appender

The file-appender receives a file path, to which it will append lines.<br/>
If the file-appender won't be used for a specific interval of time, it will flush the<br/> 
file, and no longer be available for use.
    
### Resources

In order to set the interval for non usage, a configuration file in the `resources` directory<br/>
should exist and should contain a key/value pair of:
```
{termination_interval_ms, "{interval in milliseconds}"}.
```  
By default, the expected configuration file name should be `app.config`, although the app<br/>
allows to override the default name.<br/>
In case the file would not be found, the default configuration of 5 seconds will be used.

### API

In order to use the file-appender you need to call the `start` method:

```erlang
Pid = file_appender.start("{absolute path of the file}").
```

You can also call an overloaded version of `start` which contains another parameter of type Map.<br/>
The map should be used to change the default configuration file if needed.

```erlang
Pid = file_appender.start("{absolute path of the file}", #{ conf => "{other confg absolute file path}" }).
```  

and then you can call the `append_line` method:

```erlang
file_appender.append_line(Pid, "{your desired line}").
``` 

Please note that you don't need to use the line feed `\n` in your input, the file-appender will do it for you.<br/> 
In case your input contains line feed `\n` at the end, it will not emit a line feed for this input.    


### The following are prerequisites to running the application:

1. [OTP 23](https://www.erlang.org/downloads)
2. [rebar3](https://www.rebar3.org/)
3. [Erlang intellij plugin](https://plugins.jetbrains.com/plugin/7083-erlang) in case you intend to run the code in intellij

### Compiling via erlc:

- cd {path to the root directory}
- mkdir out
- cd out 
    
```erlang
 erlc -pa {path to the root directory}  {path to the root directory}/src/utils.erl 
 {path to the root directory}/src/file_appender.erl 
 {path to the root directory}/src/file_appender_tests.erl
```

### Running:

- Via erl:
    - [Compiling via erlc](https://github.com/sagifogel/file-appender#compiling-via-erlc)
    - cd {path to the root directory}
    - `erl -pa {path to the root directory}//out/production/file-appender -pa {path to the root directory}/file-appender`
    - Use the [API](https://github.com/sagifogel/file-appender#API)
- Via Intellij: 
    - Load the project
    - Go to "Project Structure" -> "Modules"
    - Remove the current file-appender module using the minus sign.
    - Push the + sign -> and choose "New Module"
    - Choose erlang module
    - Choose the Project SDK
    - In the new module window enter these values:
        - Module Name - file-appender
        - Content root - current directory - {path}\file-appender for example `C:\sagi\github\file-appender`
        - Module file location - same as Content root
    - Select the `Add Configuration` tool -> push `+` sign to add a new app -> Erlang Console
    - Enter `file-appender-console` in the `Name` -> Ok
    - Push the Play button
    - Use the [API](https://github.com/sagifogel/file-appender#API/)
    
### Testing:

- Via erl
   - [Compiling via erlc](https://github.com/sagifogel/file-appender#compiling-via-erlc)
   - cd {path to the root directory}
   - `erl -pa {path to the root directory}/out -pa {path to the root directory}`
   -  `file_appender_tests:test().`
- Via Intellij:
   - Find the file `file_appender_tests.erl`
   - Right click -> Create 'file_appender_tests'... -> Ok
   - Run using the play button