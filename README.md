# faclig

![thumbnail](docs/img/289767.gif)

Rendering ASCII Art(AA) model that follows Face tracking data on CUI
This aims to be [Live2D](https://www.live2d.com/) for AA
This is designed as [Cj-bc/FDS-protos](https://github.com/Cj-bc/FDS-protos) front end.
Still under development.


# feature

- AA model format
- Apply tracking data to model

# How to use this

This require to run [Cj-bc/Face-Data-Server](https://github.com/Cj-bc/Face-data-server)
(or other implementation of [Cj-bc/FDS-protos](https://github.com/Cj-bc/FDS-protos) back end).
Once it's up, execute command along with faclig model path(format in [faclig_format.md -- en](docs/en/faclig_format.md))


```shell
# run FDS backend
# Please refer to the link above
$ face-Data-Server &
# execute faclig with path
$ faclig resources/face.faclig.yaml
```

## how to run

```shell
# without installing faclig
$ stack run -- <PATH>
# after installing faclig
$ faclig <PATH>
```


# how to install

