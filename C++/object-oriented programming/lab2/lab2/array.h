#pragma once
#include <iostream>
using namespace std;
//подключение необходимых библиотек

class Array
{
	int* a, n;

	void ShiftLeft(int pos);		//сдвиг влево, начиная с позиции pos+1, 
	//необязательный метод!

public:
	Array();
	Array(int);
	//вместо Array() и Array(int) можно сделать один
	//конструктор с аргументом по умолчанию:
	//Array(int m=1);
	Array(int* b, int m);		//m -число эл-тов в массиве b

	Array(const Array&);
	Array& operator= (const Array&);
	~Array();

	void Scan(int m);  			//ввод массива из m эл-тов с клавиатуры
	void Print();      			//вывод массива на консоль

	int& operator [] (int);
	int FindKey(int);  			//поиск эл-та в массиве. Возвращает индекс 
	//эл-та или -1, если эл-т в массиве отсутствует


	Array& operator+= (int);	 	//добавляет эл-т в конец массива *this
	Array  operator+ (int key);		//создаем новый массив b=*this+key

	Array& operator+= (Array X);	//добавляет массив X в конец массива *this
	Array  operator+ (Array X);		//создаем новый массив b=*this+X

	Array& operator-= (int);		//удаляем эл-т из массива *this
	Array  operator- (int key);		//создаем новый массив без эл-та key

	Array& DelPosEq(int);			//удаление эл-та с заданной позиции
	//массива *this
	Array DelPosNew(int);			//создаем новый массив, удалив эл-т
	//с заданной позиции массива *this

	bool operator== (Array);
	bool operator!= (Array);

	int Max(); 					//возвращает индекс max элемента
	int Min(); 					//возвращает индекс min элемента
	void Sorting();  				//сортировка массива

	friend ostream& operator<< (ostream& r, Array& X);
	friend istream& operator>> (istream& r, Array& X);

	int GetN() { return n; }
	operator int();    				//можно не вводить, если есть метод GetN
};
