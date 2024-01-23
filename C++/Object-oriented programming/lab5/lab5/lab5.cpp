#include "SBase.h"
#include "Freq.h"

int main() {
    srand(time(0));
    // Использование класса SFile
    SFile fileReader("Test.txt");
    Freq freq1;
    freq1.Calc(fileReader);
    cout << "Frequencies from FILE:" << endl;
    cout << freq1;

    // Использование класса SKbrd
    SKbrd keyboardReader;
    Freq freq2;
    freq2.Calc(keyboardReader);
    cout << "Frequencies from KEYBOARD input:" << endl;
    cout << freq2;

    // Использование класса SQueue
    const int queueLength = 10;
    SQueue queue(queueLength);
    Freq freq3;
    freq3.Calc(queue);
    cout << "Frequencies from QUEUE:" << endl;
    cout << freq3;

    // Использование класса Diap
    Diap diap;
    diap.Calc(fileReader);
    cout << "Range from file:" << endl;
    cout << diap;

    return 0;
}
