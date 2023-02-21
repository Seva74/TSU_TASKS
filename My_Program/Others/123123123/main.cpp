#include <iostream>
#include <new>

int main()
{
    int *arr = new int[10];
    int *ptr = arr;
    for (int i = 0; i < 10; i++)
    {
        *(ptr + i) = i * i;
    }

    for (int i = 0; i < 10; i++)
    {
        std::cout << "arr[" << i << "] = " << *(ptr + i) << std::endl;
    }
}
