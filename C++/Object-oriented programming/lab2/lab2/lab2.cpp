#include <iostream>
#include "array.h" // Подключаем класс "Массив"

int main() {
	cout << "    1 exercise" << endl;
	Array mas1;
    mas1.Scan(5);
	cout << "mas1 = "; mas1.Print();
	cout <<endl<< "    2 exersice" << endl;
	Array mas2(mas1);
	cout << mas2;
	cout << endl << "    3 exersice" << endl;
	if (mas1 == mas2) {
		cout <<"mas1 is equal to mas2";
	}
	else
	{
		cout << "mas1 is not equal to mas2";
	}

	cout << endl << "    4 exersice" << endl;
	mas1 += 6;
	cout << " mas1 = "; mas1.Print();
	if (mas1 == mas2) {
		cout << endl << "mas1 is equal to mas2";
	}
	else
	{
		cout <<endl << "mas1 is not equal to mas2";
	}
	cout << endl << "    5 exersice" << endl;
	Array mas3(mas1);
	mas3 += 7;
	cout << "mas3 = "; mas3.Print();
	cout << endl << "    6 exersice" << endl;
	mas3.DelPosEq(0);
	cout << "mas3 = "; mas3.Print();
	cout << endl << "    7 exersice" << endl;
	Array mas4(mas3); 
	mas4.DelPosEq(mas3.GetN() - 1);
	cout << "mas4 = "; mas4.Print();
	cout << endl << "    8 exersice" << endl;
	mas1 -= 1;
	if (mas1 == mas4) {
		cout << "mas1 is equal to mas4";
	}
	else {
		cout << "mas1 is not equal to mas4";
	}
	cout << endl << "    9 exersice" << endl;
	int n;
	std::cout << "enter the number of array elements: ";
	std::cin >> n;
	int* b = new int[n];
	for (int i = 0; i < n; i++) {
		b[i] = rand(); 
	}
	Array mas5(b, n);
	std::cout << "mas5: ";
	mas5.Print();
	delete[] b;
	cout << endl << "    10 exercise" << endl;
	//int maxElement5 = mas5.Max();
	//int minElement5 = mas5.Min();
	/*int indexOfMax5 = mas5.FindKey(mas5.Max());
	int indexOfMin5 = mas5.FindKey(mas5.Min());*/
	cout << "Max Element in mas5: " << mas5.Max() << " at index: " << mas5.FindKey(mas5.Max()) << endl;
	cout << "Min Element in mas5: " << mas5.Min() << " at index: " << mas5.FindKey(mas5.Min());
	cout << endl << "    11 exercise" << endl; 
	mas5.Sorting();
	cout << "Sorted mas5: "; mas5.Print();
	cout << endl << "    12 exercise" << endl;
	Array mas6(mas1 + mas5);
	cout << "mas6: "; mas6.Print();
	cout << endl << "    13 exercise" << endl;
	Array mas7(4);
	cin >> mas7;
	cout << "mas7 = " << mas7;
	cout << endl << "    14 exercise" << endl;
	cout << "Is 1 in the mas7? ";
	if (mas7.FindKey(1) >= 0) cout << "Yes. ";
	else cout << "No.";
	cout << "Is 10 in the mas7? ";
	if (mas7.FindKey(10) >= 0) cout << "Yes. ";
	else cout << "No";
	Array mas8(mas7 -= 10);
	cout << "mas 8 = "; mas8.Print();
	cout << endl << "    15 exercise" << endl;
	mas4 = mas4 + mas7;
	cout << "mas4 = "; mas4.Print();
	if (mas6 == mas4) {
		cout << "mas6 is equal to mas4";
	}
	else {
		cout << "mas6 is not equal to mas4";
	}
	cout << endl << "    16 exercise" << endl;
	mas4 = mas6; 
	cout << "mas4: "; mas4.Print();

	// 1 2 3 4 5
    return 0;
}