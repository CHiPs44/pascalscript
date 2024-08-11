// Converted with https://www.codeconvert.ai/pascal-to-c-converter

#include <stdio.h>

int recursive_factorial(int n) {
    if (n <= 1) {
        return 1;
    } else {
        return n * recursive_factorial(n - 1);
    }
}

int iterative_factorial(int n) {
    int i, f;
    if (n <= 1) {
        f = 1;
    } else {
        f = 1;
        for (i = 2; i <= n; i++) {
            f = f * i;
        }
    }
    return f;
}

int main() {
    int n;
    do {
        printf("N=");
        scanf("%d", &n);
    } while (n <= 0);
    
    printf("Recursive: %d! = %d\n", n, recursive_factorial(n));
    printf("Iterative: %d! = %d\n", n, iterative_factorial(n));
    
    return 0;
}
