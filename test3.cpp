#include <string>
#include <stdio.h>

int main() {
    int broj = 1;
    switch (broj) {
        case 1:
            printf("case1a");
            printf("case1b");
            break;
        case 2:
            printf("case2");
        case 3:
        case 4:
        case 5:
            printf("case345");
            break;
        default:
            printf("KRAJ");
            break;
    }
    return 0;
}
