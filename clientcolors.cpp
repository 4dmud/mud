/* This code started to be written: Mon May 30th 2005 - Mordecai */
/**
Want to display a configuration page for colour, simmlar to the one for game variables.

These settings will applied after loading the character, as color elements
in a 'stylesheet' for the game.
for example:
d->Output( MXPTAG("!ELEMENT RoomTitle '<COLOUR %s><B>'"), d->character->mxpcolour->RoomTitle);

There is a list of colours that work available in:
const char * colour_option_list[];
TODO: These colours have spaces in them and should have their spaces removed.

Room:
Name
Exits
Description

NPCs:
Short Desc
Long Desc
look desc

Objects:
Short Desc
Long Desc/Title
Extra Descriptions
Smell
Feel
Taste

Channels (all)


**/
