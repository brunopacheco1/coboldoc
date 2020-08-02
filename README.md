<h1 align="center">
  <br>
    <img src="https://github.com/brunopacheco1/coboldoc/blob/master/icon.png?raw=true" alt="logo" width="200">
  <br>
  COBOL Documentation CLI
  <br>
  <br>
</h1>

<h4 align="center">Command-line tool for generating COBOL documentation</h4>

#### Features
- Generate documentation in Markdown;
- Generate documentation in HTML;

```
$ coboldoc 
                 _               _       _                
   ___    ___   | |__     ___   | |   __| |   ___     ___ 
  / __|  / _ \  | '_ \   / _ \  | |  / _` |  / _ \   / __|
 | (__  | (_) | | |_) | | (_) | | | | (_| | | (_) | | (__ 
  \___|  \___/  |_.__/   \___/  |_|  \__,_|  \___/   \___|
                                                          
Usage: coboldoc <command> [options]

Options:
  -d, --dialect <dialect>         The suported dialects are FREE (default: "FREE")
  -o, --output <outputDirectory>  The output directory (default: "./")
  -f, --format <fileFormat>       The suported output format are MD or HTML (default: "MD")
  -v, --verbose                   For extra logs to help on debugging
  -v, --version                   output the version number
  -h, --help                      display help for command

Commands:
  generate <files...>             generate the documentation
  help [command]                  display help for command
```

#### Requirements
- NodeJS 8+

#### Installation
```
$ npm install -g coboldoc
```

Your contribution is always welcome!