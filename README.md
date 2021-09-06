# faclig

![thumbnail](docs/img/289767.gif)

Rendering ASCII Art(AA) model that follows [VMC Protocol](https://protocol.vmc.info/)
data on CUI.

This aims to be [Live2D](https://www.live2d.com/) for AA
Still under development.


# feature

- AA model format
- Apply tracking data to model

# How to use this

This require to run one of external application(e.g. [Waidayo](https://booth.pm/ja/items/1779185), [virtual motion capture](https://vmc.info/)
which faclig will recieve VMC protocol from.

You can find some application from [vmc.info page](https://protocol.vmc.info/Reference)

Once it's up, execute command along with faclig model path(format in [faclig_format.md -- en](docs/en/faclig_format.md))

## how to run

```shell
# without installing faclig
$ stack run -- <PATH>
# after installing faclig
$ faclig <PATH>
```


# how to install

