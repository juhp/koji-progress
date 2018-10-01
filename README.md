Koji-progress is a tool that shows the progress of active koji builds
by checking the size of their build.log files

# Installation

```
$ git clone https://github.com/juhp/koji-progress
$ cd koji-progress
$ cabal install
```

# Usage

$ ~/.cabal/bin/koji-progress 29986248  # ← Koji taskid
qemu-3.0.0-1.fc29.src.rpm
aarch64 7.2M open
x86_64 18M open
ppc64le 15M open
```

# Todo

- access task or build url
- watch until finish
- compare sizes with previous build
