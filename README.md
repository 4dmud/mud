# 4 Dimensions MUD

## Compile and run in container
```bash
$ docker build -t 4dmud .
$ docker run -d --rm -p6000:6000 --name 4d 4dmud
$ telnet localhost 6000

# view logs
$ docker exec -it 4d tail -f log/syslog
```

## Compile and run locally
```bash
# Unpack world.tar:
$ tar -xf world.tar
# Compile the source files
$ make
# Start the process in the background
$ bin/circle&

# Connect to the game
$ telnet localhost 6000
```

There are two characters available: `imm` (an implementor), and `mortal` (a regular player).
Login password is: `password` for both characters.

NB: If you edit any header `*.h` file, you need to `make clean` and recompile from scratch `make`.

## Pull Request Process
1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add new feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull/Merge Request

## Contribution Code of Conduct
When contributing to this project; first discuss the change you wish to make via github issue.

Note we have a [code of conduct](./CODE_OF_CONDUCT.md), please follow it in all your interactions with the project.

>Credits: This code is based on CircleMUD 3.1 by Jeremy "Ras" Elson, who based it on DikuMUD.
