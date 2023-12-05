#include <iostream>
#include <cmath>
#include <locale.h>

class RightTriangle {
private:
    double xA, yA;
    double a, b;

public:
    RightTriangle() : xA(0.0), yA(0.0), a(0.0), b(0.0) {}

    void input() {
        std::cout << "Введите координаты точки A (x y): ";
        std::cin >> xA >> yA;
        std::cout << "Введите длину катета a: ";
        std::cin >> a;
        std::cout << "Введите длину катета b: ";
        std::cin >> b;
    }

    void output() {
        std::cout << "Координаты точки A: (" << xA << ", " << yA << ")\n";
        std::cout << "Длина катета a: " << a << "\n";
        std::cout << "Длина катета b: " << b << "\n";
    }

    double calculateHypotenuse() {
        return sqrt(a * a + b * b);
    }

    double calculateArea() {
        return 0.5 * a * b;
    }

    double calculatePerimeter() {
        return a + b + calculateHypotenuse();
    }

    double calculateInscribedCircleRadius() {
        return calculateArea() / (a + b + calculateHypotenuse());
    }

    double calculateCircumscribedCircleRadius() {
        return calculateHypotenuse() / 2;
    }

    double calculateHeight() {
        return (2 * calculateArea()) / calculateHypotenuse();
    }

    double calculateSmallerAngle() {
        return asin(a / calculateHypotenuse());
    }

    bool isIsosceles() {
        return a == b;
    }

    void multiply(double factor) {
        a *= factor;
        b *= factor;
    }

    bool isEqual(const RightTriangle& other) {
        return (xA == other.xA && yA == other.yA && a == other.a && b == other.b);
    }

    bool isSimilar(const RightTriangle& other) {
        double ratioA = a / other.a;
        double ratioB = b / other.b;
        return (ratioA == ratioB);
    }

};

int main() {
    setlocale(0,"");
    RightTriangle triangle;
    triangle.input();
    triangle.output();

    std::cout << "Гипотенуза: " << triangle.calculateHypotenuse() << "\n";
    std::cout << "Площадь: " << triangle.calculateArea() << "\n";
    std::cout << "Периметр: " << triangle.calculatePerimeter() << "\n";
    std::cout << "Радиус вписанной окружности: " << triangle.calculateInscribedCircleRadius() << "\n";
    std::cout << "Радиус описанной окружности: " << triangle.calculateCircumscribedCircleRadius() << "\n";
    std::cout << "Высота, опущенная на гипотенузу: " << triangle.calculateHeight() << "\n";
    std::cout << "Меньший угол в треугольнике (радианы): " << triangle.calculateSmallerAngle() << "\n";
    std::cout << "Равнобедренный треугольник: " << (triangle.isIsosceles() ? "Да" : "Нет") << "\n";
    //std::cout << "Равнобедренный треугольник: " << triangle.isIsosceles() << "\n";

    double factor;
    std::cout << "Введите число для умножения треугольника: ";
    std::cin >> factor;
    triangle.multiply(factor);
    std::cout << "Параметры треугольника после умножения:\n";
    triangle.output();

    RightTriangle anotherTriangle;
    anotherTriangle.input();

    std::cout << "Треугольники равны: " << (triangle.isEqual(anotherTriangle) ? "Да" : "Нет") << "\n";
    std::cout << "Треугольники подобны: " << (triangle.isSimilar(anotherTriangle) ? "Да" : "Нет") << "\n";

    return 0;
}
