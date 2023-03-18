# Setting up the environment
Follow [SBT Manual](https://www.scala-sbt.org/1.x/docs/Setup.html) to install SBT, ensuring **JDK 11** is installed. Follow the instructions in **Scala Stuff** to get the hardfloat dependency. Run `source env.sh` and you should be able to compile models using the `swiftstatcc` command.

# Running a Basic Compile
Use the demo models in the `models` directory. For example, to compile the `student2` network:
```
    swiftstatcc --pgm models/student2.pgm
```
The MDL will be output in protobuf/JSON format and the generated hardware (FIRRTL/Verilog) will be placed in the `swiftstat_generated/` folder.

# Testing
`sbt test` will run everything. `sbt "testOnly *<Class>"` will let you run a specific test class.

## Adding tests
Mirror the directory structure of the thing you are testing.

## Coverage
```
    sbt clean coverage test
    sbt coverageReport
```
The report is available at
```
    target/scala-2.13/scoverage-report/index.html
```

# VSCode Extension
This does some syntax highlighting on `.pgm` files.

## Install
```
    cp -r vscode_extension/swiftstat-pgm ~/.vscode/extensions
```

## Uninstall
```
    rm -rf ~/.vscode/extensions/swiftstat-pgm
```

# Scala Stuff

## Hardfloat
There is a dependency on the [Berkeley hardfloat](https://github.com/ucb-bar/berkeley-hardfloat) library, which as of writing isnt published anywhere. According to [this thread](https://stackoverflow.com/questions/44189870/using-berkeley-hardfloat), the current best way to handle this is build and deploy it locally (sad!). In a separate directory:

```
    git clone https://github.com/ucb-bar/berkeley-hardfloat.git hardfloat
    cd hardfloat
    sbt publishLocal
```

## Chisel Usage Notes

- when declaring an `io` bundle, its best to give _meaningful_ names to the fields. This allows adding/removing fields in a sensible way in the future (dont just call them `in` and `out`!)
- when testing and using FloatUtils to inspect `RecFN`s, ensure that you `peek()` before `toDouble`ing. Otherwise you will get a 'None' value in the litValue field, which will throw an exception when you try to convert it to a double.
- don't forget to declare `io` before other construction stuff that depends on `io`
- don't use underscores in signal names. This can cause issues with the verilator backend due to name encoding (https://verilator.org/guide/latest/languages.html)

## Visualiser

https://www.chisel-lang.org/diagrammer/

```
    git clone https://github.com/freechipsproject/diagrammer
    cd diagrammer
    ./diagram.sh [...]/SwiftStatNetwork.fir
```

You can then open `SwiftStatNetwork_hierarchy.dot.svg` in your browser to look at the module!