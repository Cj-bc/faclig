# Faclig model format

This document defines ASCII Art model format for Faclig.

# version

Current version is 0.1.0

Using [semantics versioning 2.0.0](https://semver.org/spec/v2.0.0.html).

# overview

The model is consist of two kind of files:
  - `faclig format` file, which contains metadata and infos how to construct models from parts
  - `part` file, which actually just [Shgif](https://github.com/Cj-bc/brick-shgif) file


## `faclig format` file

### Overview

This file defines:
  - name of the model
  - author of the model
  - version of model format
  - `part information`s

#### Yaml format and example

Yaml format is:
```yaml
name: <name of the model, Text>
author: <name of the creater, Text>
version: <version number, Text>
parts: # describe this below
  ...
```

Example:
```yaml
name: haskell girl
author: Cj.BC_SD a.k.a Cj-bc
version: 0.1.0
parts:
  ...
```

### Part information

`Part information` is:
  - which file does hold part A Shgif?
  - How much offset does part A need?

#### What is offset

_offset_ is List of Two Int.
It specify where to render the part.
First one is X offset, 2nd is Y offset.  
If the list is longer than 2, they'll be just discarded.  
The __Left-Top__ coordinate is `(0, 0)`

#### Yaml format and example

Yaml format is:
```yaml
<part_name>:
  path: <Path_to_the_shgif, Text>
  offset: [] <Offset of the part, [Int, Int]>
```

Example, 'leftEye' part:
```yaml
leftEye:
  path: "leftEye.yaml"
  offset: [0, 0]
```

#### Defined Parts

Currently, 7 parts are defined and __required__.

| part name | description |
| :-:|:-:|
| contour | Face contour, in other word, face basement |
| leftEye | Left eye |
| rightEye | Right eye |
| nose     | Nose   |
| mouth    | Mouth  |
| hair     | front hair |
| backHair | back hair, which will be rendered behind contour |


## `part file`

Part file is just Shgif file for each part.  
Please refer to [Cj-bc/brick-shgif -- docs/shgif-format.md](https://github.com/Cj-bc/brick-shgif/blob/master/docs/shgif-format.md) for the format.  
Currently(at 2020/03/12) faclig use `v1.0.0` definition of Shgif.
