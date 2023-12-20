#include "Cstr.h"
#include "CStrArray.h"
#include <iostream>
using namespace std;

int main()
{
    srand(time(0));
    CStr str1; // конструктор по умолчанию, создающий случайную строку
    CStr str2("Hello world"); // онструктор с параметром "строка"
    CStr str3(8); // конструктор с параметром "длина строки"

    cout << "str1: " << str1 << endl;
    cout << "str2: " << str2 << endl;
    cout << "str3: " << str3 << endl;

    str1 = str2; // оператор присваивания объекта CStr
    cout << "After assignment str1 = " << str1 << endl;

    if (str2 > str3) // оператор сравнения > (или <) объектов CStr
        cout << "str2 is longer than str3" << endl;
    else
        cout << "str2 is not longer than str3" << endl;


    if (str2 == str1) // оператор проверки равенства (==) объектов CStr
        cout << "str2 is equal to str1" << endl;
    else
        cout << "str2 is not equal to str1" << endl;

    cout << "Length of str1: " << str1.get_length() << endl << endl;
 




    // объект CStrArray с массивом заданной длины
    int arrayLength = 10;
    CStrArray strArray(arrayLength);

    // Заполняем массив случайными строками
    for (int i = 0; i < arrayLength; ++i)
    {
        strArray[i] = CStr(); // Используется метод generate для создания случайной строки
    }

    // Выводим неотсортированный массив
    std::cout << "Original array:\n" << strArray << std::endl;

    // Сортировка по длине строк
    strArray.sort_by_length();
    std::cout << "Sorted by length:\n" << strArray << std::endl;

    // Сортировка по содержимому строк
    strArray.sort_by_content();
    std::cout << "Sorted by content:\n" << strArray << std::endl;



    // Проверка упорядоченности массива
    if (strArray.check_sort())
        std::cout << "Array is ordered.\n";
    else
        std::cout << "Array is not ordered.\n";

    // Бинарный поиск
    CStr searchStr = strArray[3]; // Используем строку из массива для поиска
    int index = strArray.bin_search(searchStr);
    if (index != -1)
        std::cout << "String found at index " << index << ".\n";
    else
        std::cout << "String not found.\n";

    //return 0;
}
