#include "Cstr.h"
#include <cstdlib>
#include <ctime>

// Приватный метод для генерации случайной строки заданной длины
char* CStr::generate(int length)
{
    //srand(time(0));
    char* result = new char[length + 1];
    for (int i = 0; i < length; ++i)
    {
        result[i] = 'a' + rand() % 26; // Генерация случайной строчной буквы
    }
    result[length] = '\0'; // Установка нулевого символа в конце строки
    return result;
}

// Конструктор по умолчанию
CStr::CStr()
{
    //srand(time(0));
    //srand(static_cast<unsigned int>(time(nullptr))); // Инициализация генератора случайных чисел
    int length = rand() % 20 + 1; // Генерация случайной длины от 1 до 20
    string = generate(length);
}

// Конструктор с параметром "строка"
CStr::CStr(const char* str)
{
    int length = 0;
    while (str[length] != '\0')
    {
        ++length;
    }
    string = new char[length + 1];
    for (int i = 0; i <= length; ++i)
    {
        string[i] = str[i];
    }
}

// Конструктор с параметром "длина строки"
CStr::CStr(int length)
{
    string = generate(length);
}

// Конструктор копии
CStr::CStr(const CStr& obj)
{
    int length = obj.get_length();
    string = new char[length + 1];
    for (int i = 0; i <= length; ++i)
    {
        string[i] = obj.string[i];
    }
}

// Деструктор
CStr::~CStr()
{
    delete[] string;
}

// Оператор присваивания объекта CStr
CStr& CStr::operator=(CStr& obj)
{
    if (this != &obj)
    {
        delete[] string;
        int length = obj.get_length();
        string = new char[length + 1];
        for (int i = 0; i <= length; ++i)
        {
            string[i] = obj.string[i];
        }
    }
    return *this;
}

// Оператор присваивания строки (char *)
CStr& CStr::operator=(const char* str)
{
    int length = 0;
    while (str[length] != '\0')
    {
        ++length;
    }
    delete[] string;
    string = new char[length + 1];
    for (int i = 0; i <= length; ++i)
    {
        string[i] = str[i];
    }
    return *this;
}

CStr& CStr::operator=(const CStr& obj)
{
    if (this != &obj)
    {
        delete[] string;
        int length = obj.get_length();
        string = new char[length + 1];
        for (int i = 0; i <= length; ++i)
        {
            string[i] = obj.string[i];
        }
    }
    return *this;
}

// Оператор сравнения > (или <) объектов CStr по их содержимому (сравнение строк)
bool CStr::operator>(CStr& obj)
{
    return strcmp(string, obj.string) > 0;
}

// Оператор проверки равенства (==) объектов CStr (равенства строк)
bool CStr::operator==(CStr& obj)
{
    return strcmp(string, obj.string) == 0;
}

bool CStr::operator==(const CStr& obj) const
{
    return strcmp(string, obj.string) == 0;
}

bool CStr::operator<(const CStr& obj) const
{
    return strcmp(string, obj.string) < 0;
}

// Метод, возвращающий длину строки
int CStr::get_length() const
{
    int length = 0;
    while (string[length] != '\0')
    {
        ++length;
    }
    return length;
}

// Дружественный оператор вывода в поток <<
ostream& operator<<(ostream& stream, CStr& obj)
{
    stream << obj.string;
    return stream;
}
