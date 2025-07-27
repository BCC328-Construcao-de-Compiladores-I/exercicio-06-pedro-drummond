#include <stdio.h>
#include <stdlib.h>

int main() {
        {
        const int v_x = 1;
    int v_a = 0;
    v_a = (v_x + 1);
    printf("%d\n", v_a);
    {
        const int v_x = 5;
    v_a = (v_x - 1);
    printf("%d\n", v_a);
    }
    printf("%d\n", v_x);
    }

    return 0;
}
