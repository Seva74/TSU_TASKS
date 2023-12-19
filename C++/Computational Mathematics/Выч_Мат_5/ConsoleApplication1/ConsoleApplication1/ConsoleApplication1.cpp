#include <Windows.h>
#include <stdlib.h>
#include <math.h>
#include <iostream>
#include <string>

using namespace std;

int main(void)
{
    float x;
    HDC hDC = GetDC(GetConsoleWindow());
    HPEN Pen = CreatePen(PS_SOLID, 2, RGB(255, 255, 255));  // Белый цвет линии
    SelectObject(hDC, Pen);

    // Задаем черный цвет фона и белый цвет текста
    SetBkColor(hDC, RGB(0, 0, 0));  // Черный фон
    SetTextColor(hDC, RGB(255, 255, 255));  // Белый текст

    // Оси координат
    MoveToEx(hDC, 0, 340, NULL);  // Увеличенный размер графика
    LineTo(hDC, 800, 340);  // Увеличенный размер графика
    MoveToEx(hDC, 400, 0, NULL);  // Увеличенный размер графика
    LineTo(hDC, 400, 680);  // Увеличенный размер графика

    // Подписи осей
    TextOutW(hDC, 780, 335, L"X", 1);  // Используйте L перед строкой для широкого формата
    TextOutW(hDC, 410, 5, L"Y", 1);

    // Значения на осях
    for (int i = -8; i <= 8; ++i) {
        wstring xValue = to_wstring(i);
        TextOutW(hDC, 40 * i + 395, 350, xValue.c_str(), xValue.length());

        wstring yValue = to_wstring(-i);
        TextOutW(hDC, 395, 40 * i + 335, yValue.c_str(), yValue.length());
    }

    for (x = -8.0f; x <= 8.0f; x += 0.01f)
    {
        MoveToEx(hDC, 40 * x + 400, -40 * (x + cos(x) - 2) + 340, NULL);  // Увеличенный размер графика
        LineTo(hDC, 40 * x + 400, -40 * (x + cos(x) - 2) + 340);  // Увеличенный размер графика
    }

    // Ожидание пользовательского ввода перед завершением программы
    cout << "Graph x + cos(x) - 2 = 0";
    getchar();

    return 0;
}
