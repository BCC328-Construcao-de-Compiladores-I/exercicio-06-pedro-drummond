#include <stdio.h>
#include <stdlib.h>

int main() {
        {
    const int x = 1;
    int a = 0;
    a = (x + 1);
    printf("%d\n", a);
    {
    const int x = 5;
    a = (x - 1);
    printf("%d\n", a);
    }
    printf("%d\n", x);
    }

    return 0;
}
