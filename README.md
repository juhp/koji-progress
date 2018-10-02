Koji-progress is a tool that shows the progress of active koji builds tasks
by checking the size of their build.log files.

This is useful for monitoring the build progress of large packages that take a long time to complete for which some arch's may take considerably longer.

# Installation

```
$ git clone https://github.com/juhp/koji-progress
$ cd koji-progress
$ cabal install
```

# Usage

```
$ koji list-tasks --mine
:
$ ~/.cabal/bin/koji-progress 29986248  # ← Koji taskid
qemu-3.0.0-1.fc29.src.rpm
aarch64 7.2M open
x86_64 18M open
ppc64le 15M open
```

# Todo

- accept task or build url
- watch tasks until complete
- compare sizes with previous build
