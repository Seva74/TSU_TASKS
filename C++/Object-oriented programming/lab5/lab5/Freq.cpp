#include "Freq.h"

void Freq::Calc(SBase& object)
{
    int number;
    sum = 0;
    while ((number = object.Get()) >= 0) {
        counts[number]++;
        sum += number;
    }
}


ostream& operator<<(ostream& stream, Freq& object)
{
    for (const auto& pair : object.counts) {
        stream << pair.first << ": " << pair.second << endl;
    }
    stream << "Sum: " << object.sum << endl << endl;
    return stream;
}


void Diap::Calc(SBase& object)
{
    int number;
    max = INT_MIN;
    min = INT_MAX;
    count = 0;

    // Закрыть и снова открыть файл
    if (dynamic_cast<SFile*>(&object) != nullptr) {
        SFile& fileObject = dynamic_cast<SFile&>(object);
        fileObject.~SFile(); // Закрыть файл
        new (&fileObject) SFile("Test.txt"); // Снова открыть файл
    }

    while ((number = object.Get()) != -1) {
        if (number < min) {
            min = number;
        }
        if (number > max) {
            max = number;
        }
        count++;
    }
}


ostream& operator<<(ostream& stream, Diap& object)
{
    stream << "Min Number: " << object.min << endl;
    stream << "Max Number: " << object.max << endl;
    stream << "Count: " << object.count << endl;
    return stream;
}