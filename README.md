# faclig -- Fake facerig in CLI

  This is a project to attempt to make facerig in CLI.
  Of cource we will use ASCII arts instead of drawings.

# deps

This repository depends on:
  * Cj-bc/libdraw
  * Cj-bc/shgif
  * openCV

# install

I'll make another way to install, but for now, for reference:

```bash
$ brew install opencv
$ bpkg install Cj-bc/libdraw
```

# usage

```bash
$ faclig datadir
# which datadir should looks like this
#   datadir --+
#             +- faclig.conf
#             +- src ---------+
#                             +- eye_left.txt
#                             +- eye_blink.tar
#                                 ...
```
