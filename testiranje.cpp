#include <string>
#include <stdio.h>

int main() {
    int broj = 1;
    switch (broj) {
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

    switch (int f = 3) {
        case 2:
            printf("asdf 2");
            break;
        case 3:
            printf("asdf 3");
            break;
    }
    return 0;
}

