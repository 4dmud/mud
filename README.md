# 4 Dimensions MUD

## Contribute
When contributing to this project; first discuss the change you wish to make via issue, email, or any other method with the maintainers before submitting changes.

Note we have a [code of conduct](./CODE_OF_CONDUCT.md), please follow it in all your interactions with the project.

## Compile and run
Unpack world.tar in the /src dir:

`$ tar -xf world.tar -C ../`

`$ make`

`cd ..`

`bin/circle&` (this will start the mud)

Now you can connect with your mudclient to localhost port 6000.
There are two characters available: 'imm', an implementor, and 'mortal'.
Both of their passwords are set to 'password'.

## Pull Request Process
1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add new feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull/Merge Request

Note: If you need to edit a header (.h) file, then you need to 'make clean'
      before doing 'make'.


>Credits: This code is based on CircleMUD 3.1 by Jeremy "Ras" Elson, who based it on DikuMUD.
