#include <iostream>
#include <fstream>
using namespace std;

int a[15][15], b[15][15];
int main()
{
    ifstream in ("18.xlsx");
    for (int i = 0; i <= 14; i++)
    {
        for (int j = 0; j <= 14; j++)
        {
            in >> a[i][j];
        }
    }


    for (int j = 14; j > 0; j--) b[0][j] = a[0][j] + b[0][j + 1]; 
    for (int i = 0; i <= 14; i++) b[i][14] = a[i][14] + b[i - 1][14];


    for (int i = 1; i <= 14; i++)
    {
        for (int j = 13; j >= 0; j--)    b[i][j] = a[i][j] + max(b[i - 1][j], b[i][j + 1]);
    }
    cout << a[13][3];

}
