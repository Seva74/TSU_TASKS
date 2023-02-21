// 2.2.cpp : Этот файл содержит функцию "main". Здесь начинается и заканчивается выполнение программы.
//

#include <iostream>
using namespace std;
int main()
{
    int a;
    cin >> a;
    cout << "The next number for the number " << a << " is " << (a + 1) << ".";
    cout << "\n" << "The previous number for the number " << a << " is " << (a - 1) << ".";
    return 0;
}
