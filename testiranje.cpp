#include <string>
#include <stdio.h>

int main(){
	int broj = 1;
	switch(broj){
		case 1:
			printf("ASD1a");
			printf("ASD1b");
			break;
		case 2:
		case 3:
		case 4:
		case 5:
			printf("ASD2 or ASD3");
			break;
		default:
			printf("KRAJ");
			break;
	}
	return 0;
}

