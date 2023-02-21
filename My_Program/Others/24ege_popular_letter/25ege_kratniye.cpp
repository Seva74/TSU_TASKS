#include <iostream>
#include <fstream>
using namespace std;

int n, A, E;
char s, a[1023];
int main()
{

    ifstream in("24 (1).txt");
            for (int i = 0; i < 1000; i++)
            {
                if (a[i] == 'A')A += 1;
                if (a[i] == 'E')E += 1;
                in.getline(a, 1023);
            }
        cout << (E - A);
}   
