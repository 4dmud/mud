#include <stdio.h>
extern char last_command[];

void print_last_command() {
	printf("the last command typed was: \"%s\"\n",last_command);
}
