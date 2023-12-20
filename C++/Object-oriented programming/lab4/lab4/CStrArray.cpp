#include "CStrArray.h"
#include <algorithm>

CStrArray::CStrArray(int leng)
{
    length = leng;
    arr = new CStr[length];
}

CStrArray::~CStrArray()
{
    delete[] arr;
}

CStr& CStrArray::operator[](int index)
{
    return arr[index];
}

void CStrArray::sort_by_content()
{
    // —ортировка методом пузырька (можете выбрать другие алгоритмы)
    for (int i = 0; i < length - 1; ++i)
    {
        for (int j = 0; j < length - i - 1; ++j)
        {
            if (arr[j] > arr[j + 1])
            {
                std::swap(arr[j], arr[j + 1]);
            }
        }
    }
}

void CStrArray::sort_by_length()
{
    // —ортировка методом пузырька по длине строк
    for (int i = 0; i < length - 1; ++i)
    {
        for (int j = 0; j < length - i - 1; ++j)
        {
            if (arr[j].get_length() > arr[j + 1].get_length())
            {
                std::swap(arr[j], arr[j + 1]);
            }
        }
    }
}

int CStrArray::bin_search(const CStr& str)
{
    int left = 0;
    int right = length - 1;

    while (left <= right)
    {
        int mid = left + (right - left) / 2;

        if (arr[mid] == str)
            return mid; // —трока найдена, возвращаем индекс

        if (arr[mid] < str)
            left = mid + 1;
        else
            right = mid - 1;
    }

    return -1; // —трока не найдена
}

bool CStrArray::check_sort()
{
    // ѕроверка упор€доченности массива по содержимому строк
    for (int i = 0; i < length - 1; ++i)
    {
        if (arr[i] > arr[i + 1])
        {
            return false;
        }
    }
    return true;
}

ostream& operator<<(ostream& stream, CStrArray& obj)
{
    for (int i = 0; i < obj.length; ++i)
    {
        stream << obj.arr[i] << endl;
        
    }
    return stream;
}
