using System;
using System.Collections.Generic;
using System.Linq;

public class Item
{
    public string Name { get; set; }
    public double Price { get; set; }

    public Item(string name, double price)
    {
        Name = name;
        Price = price;
    }

    public override string ToString()
    {
        return $"{Name} - {Price} рублей";
    }
}

public class ShoppingCart
{
    private List<Item> items = new List<Item>();
    private string discountChoice;

    public void AddItem(Item item)
    {
        items.Add(item);
    }

    public double GetTotalAmount()
    {
        return items.Sum(item => item.Price);
    }

    public void SetDiscountChoice(string choice)
    {
        discountChoice = choice;
    }

    public double GetFinalTotal()
    {
        double total = GetTotalAmount();
        double discount = 0;

        switch (discountChoice)
        {
            case "1": // Оплата по полной цене
                discount = 0;
                break;
            case "2": // Скидка 15%
                discount = total * 0.15;
                break;
            case "3": // Промокод TSUSHOP2025 (500 рублей)
                discount = Math.Min(500, total);
                break;
            default:
                discount = 0;
                break;
        }

        return total - discount;
    }

    public string GetDiscountDescription()
    {
        switch (discountChoice)
        {
            case "1":
                return "Без скидки";
            case "2":
                return "Скидка 15%";
            case "3":
                return "Промокод 'TSUSHOP2025' на 500 рублей";
            default:
                return "Без скидки";
        }
    }

    public void DisplayCart()
    {
        Console.WriteLine("Ваша корзина:");
        if (items.Count == 0)
        {
            Console.WriteLine("Корзина пуста.");
        }
        else
        {
            foreach (var item in items)
            {
                Console.WriteLine($"- {item}");
            }
            Console.WriteLine($"Общая сумма: {GetTotalAmount()} рублей");
        }
    }
}

class Program
{
    static void Main(string[] args)
    {
        // Список доступных товаров
        List<Item> availableItems = new List<Item>
        {
            new Item("Книга 'Лекции по основам программирования'", 600),
            new Item("Ручка 'Tomsk State University'", 150),
            new Item("Тетрадь 96 листов", 250),
            new Item("Лавандовый раф на безлактозном молоке 300мл", 400),
            new Item("Шоколадка Аленка", 100)
        };

        ShoppingCart cart = new ShoppingCart();

        Console.WriteLine("Добро пожаловать в онлайн-магазин TSUShop!");
        Console.WriteLine("Выберите товары для добавления в корзину (введите номер, 0 для завершения):\n");

        // Цикл выбора товаров
        while (true)
        {
            Console.WriteLine("Доступные товары:");
            for (int i = 0; i < availableItems.Count; i++)
            {
                Console.WriteLine($"{i + 1}. {availableItems[i]}");
            }
            Console.Write("\nВаш выбор: ");
            string input = Console.ReadLine();

            Console.Clear();

            if (input == "0")
            {
                break;
            }

            if (int.TryParse(input, out int choice) && choice >= 1 && choice <= availableItems.Count)
            {
                cart.AddItem(new Item(availableItems[choice - 1].Name, availableItems[choice - 1].Price));
                Console.WriteLine($"{availableItems[choice - 1].Name} добавлено в корзину.\n");
            }
            else
            {
                Console.WriteLine("Неверный выбор, попробуйте снова.\n");
            }

            cart.DisplayCart();
        }

        // Проверка, пуста ли корзина
        if (cart.GetTotalAmount() == 0)
        {
            Console.WriteLine("Корзина пуста. Покупка завершена.");
            return;
        }

        cart.DisplayCart();

        // Выбор скидки
        Console.WriteLine("\nВыберите стратегию оплаты:");
        Console.WriteLine("1 - Оплата по полной цене");
        Console.WriteLine("2 - Скидка 15% по акции");
        Console.WriteLine("3 - Промокод TSUSHOP2025 (500 рублей)");
        Console.Write("Ваш выбор: ");
        string discountChoice = Console.ReadLine();

        cart.SetDiscountChoice(discountChoice);

        Console.Clear();
        Console.WriteLine("\nИтог покупки:");
        cart.DisplayCart();
        Console.WriteLine($"Применена скидка: {cart.GetDiscountDescription()}");
        Console.WriteLine($"Итоговая сумма: {cart.GetFinalTotal()} рублей");
        Console.WriteLine("Спасибо за покупку в TSU Shop!");
    }
}