#include <iostream>
#include <iomanip>
#include <cstdlib>
void z1();
void z2();
using namespace std;

int main()
{
    z1();
    system("Pause");
    return 0;
}

void z1()
{
    int a[10][10], i, j, k(1), ii, jj, n(1), zi(1), zj(1), g(1);
    for (i = 0; i < 10; i++)
        for (j = 0; j < 10; j++)
            a[i][j] = 0;
    for (i = 0; i < 10; i++)
    {
        for (j = 0; j < 10; j++)
            cout << setw(3) << a[i][j];
        cout << endl;
    }
    j = 4;
    i = 5;
    while (i <= 10 && j <= 10)
    {

        switch (k)
        {
        case(1):
        {
            zi = i;
            zj = j;
            i -= n;
            break;
        }
        case(2):
        {
            zi = i;
            zj = j;
            j += n;
            break;
        }
        case(3):
        {
            zi = i;
            zj = j;
            i += n + 1;
            break;
        }
        case(4):
        {
            zi = i;
            zj = j;
            j -= n + 1;
        }
        }
        if (k == 1)
            while (zi > i)
            {
                a[zi][zj] = g;
                zi--;
                g++;
            }
        else
            if (i > 10 || i < 0 || j < 0 || j>10)
            {
                system("pause");
                break;
            }
            else
                if (k == 2)
                    while (zj < j)
                    {
                        a[zi][zj] = g;
                        zj++;
                        g++;
                    }
                else
                    if (k == 3)
                        while (zi < i)
                        {
                            a[zi][zj] = g;
                            zi++;
                            g++;
                        }
                    else
                        if (k == 4)
                            while (zj > j)
                            {
                                a[zi][zj] = g;
                                zj--;
                                g++;
                            }
        cout << "i= " << i << "j= " << j << "k= " << k;
        k++;
        if (k == 5)
        {
            k = 1;
            n += 2;
        }
        system("pause");
        system("cls");
        for (ii = 0; ii < 10; ii++)
        {
            for (jj = 0; jj < 10; jj++)
                cout << setw(3) << a[ii][jj];
            cout << endl;
        }
    }
}