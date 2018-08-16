# `behaviorcodeAuto`
Automates parts of using the
[`behaviorcode`](https://github.com/FernaldLab/behaviorcode) scripts.
In order to work, modifications to `behaviorcode` are required. The
compatible form can be found in the
[fork](https://github.com/U8NWXD/behaviorcode) of `behaviorcode`.

## Configuration
`behaviorcodeAuto` can be configured through a command-line
interface. When prompted, you can choose to use a configuration file
of your choice in the YAML format. An example configuration file is
included as [`example.yml`](example.yml).

## Running `behaviorcodeAuto`
Navigate to a folder to store the output from `behaviorcode` in. It
is recommended that the configuration files also be stored here and
that the folder be close to the directory with the data.

Run `behaviorcodeAuto` using the `source` command. For example:
`source('path/to/behaviorcodeAuto.R')`

## Legal
Copyright (c) U8N WXD <cs.temporary@icloud.com>
All Rights Reserved

Inspiration and some code came from
[`behaviorcode`](https://github.com/FernaldLab/behaviorcode), which
states that

> All code is freely available for anyone to use.

and that

> for now we offer no guarantees that it will work as expected for
> other users and assume no responsibility for any unforeseen
> consequences of its use.
