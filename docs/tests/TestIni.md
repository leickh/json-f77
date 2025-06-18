# `test.ini` | JSON-F90

The `test.ini` - file is an INI-based  file of which every test has to
have at least one. This document contains the format documentation for
the unit test side.

## Section: Test

The `[Test]` section has two values for general display texts of the
test. These two values are:

- `Name`  
    The developer-readable *kebab-case* - name of the test

- `Version`  
    The [semantic version](https://semver.org) of the test.


## Section: Test.Build

The `[Test.Build]` - section, as the name implies, is  about conveying
information to the build  system about how to build the test. It needs
to have the following values:

- `Builder`  
    Build script and language to use for the test. Currently, this can
    only be `builtin:fortran-90` for  a hard-coded Fortran 90 builder.

- `Path`  
    Relevant path for the builder. If the `builtin:fortran-90`-builder
    was chosen, this is the source root folder. The path, as any other
    path in the test.ini file, can use some [path keys](#path-keys).

## Path Keys

A *path key* is a word inside a pair of curly braces which is replaced
with a corresponding path by the build system. Currently, there are
three possible values:

- `{test}`  
    The path of the  test's root folder.  Because tests  are copied to
    another location before building and running, this must be used to
    refer to source folders that are normally accessible by the usual,
    relative paths.

- `{project}`  
    The project's root folder, the folder in which `src-f90` resides.

- `{library}`  
    The project's main source folder, the `src-f90` - folder in which
    the implementation is to be found.

## Example

```ini
[Test]
Name = "tokenize"
Version = "1.5.0"

[Test.Build]
Builder = "builtin:fortran-90"
Path = "{test}/src-f90"
```
