using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using System.Drawing;

namespace TSUShop
{
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
            return $"{Name} - {Price:F2} рублей";
        }
    }

    public interface IDiscountStrategy
    {
        double CalculateDiscount(double totalAmount);
        string GetDescription();
    }

    public class NoDiscountStrategy : IDiscountStrategy
    {
        public double CalculateDiscount(double totalAmount)
        {
            return 0;
        }

        public string GetDescription()
        {
            return "Без скидки";
        }
    }

    public class PercentageDiscountStrategy : IDiscountStrategy
    {
        private readonly double percentage;

        public PercentageDiscountStrategy(double percentage)
        {
            this.percentage = percentage;
        }

        public double CalculateDiscount(double totalAmount)
        {
            return totalAmount * (percentage / 100);
        }

        public string GetDescription()
        {
            return $"Скидка {percentage}%";
        }
    }

    public class PromoCodeDiscountStrategy : IDiscountStrategy
    {
        private readonly string promoCode;
        private readonly double discountAmount;

        public PromoCodeDiscountStrategy(string promoCode, double discountAmount)
        {
            this.promoCode = promoCode;
            this.discountAmount = discountAmount;
        }

        public double CalculateDiscount(double totalAmount)
        {
            return Math.Min(discountAmount, totalAmount);
        }

        public string GetDescription()
        {
            return $"Промокод '{promoCode}' на {discountAmount:F2} рублей";
        }
    }

    public class ShoppingCart
    {
        private List<Item> items = new List<Item>();
        private IDiscountStrategy? discountStrategy;

        public void AddItem(Item item)
        {
            items.Add(item);
        }

        public double GetTotalAmount()
        {
            return items.Sum(item => item.Price);
        }

        public void SetDiscountStrategy(IDiscountStrategy strategy)
        {
            discountStrategy = strategy;
        }

        public double GetFinalTotal()
        {
            double total = GetTotalAmount();
            if (discountStrategy != null)
            {
                double discount = discountStrategy.CalculateDiscount(total);
                return total - discount;
            }
            return total;
        }

        public string GetDiscountDescription()
        {
            return discountStrategy?.GetDescription() ?? "Без скидки";
        }
    }

    public partial class Form1 : Form
    {
        private readonly ShoppingCart cart = new ShoppingCart();
        private readonly List<Item> availableItems = new List<Item>
        {
            new Item("Книга 'Лекции по основам программирования'", 600),
            new Item("Ручка 'Tomsk State University'", 150),
            new Item("Тетрадь 96 листов", 250),
            new Item("Лавандовый раф на безлактозном молоке 300мл", 400),
            new Item("Шоколадка Аленка", 100)
        };

        public Form1()
        {
            InitializeComponent();
            cart.SetDiscountStrategy(new NoDiscountStrategy());
        }

        private void Form1_Load(object sender, EventArgs e)
        {
            foreach (var item in availableItems)
            {
                lstAvailableItems.Items.Add(item);
            }
            cmbDiscountStrategy.SelectedIndex = 0;
            UpdateUI();
        }

        private async void btnAddToCart_Click(object sender, EventArgs e)
        {
            if (lstAvailableItems.SelectedItem != null)
            {
                Item selectedItem = (Item)lstAvailableItems.SelectedItem;
                cart.AddItem(new Item(selectedItem.Name, selectedItem.Price));
                lstCartItems.Items.Add(selectedItem);

                // Анимация кнопки
                Color originalColor = btnAddToCart.BackColor;
                btnAddToCart.BackColor = Color.LightGreen;
                await System.Threading.Tasks.Task.Delay(100);
                btnAddToCart.BackColor = originalColor;

                UpdateUI();
            }
            else
            {
                MessageBox.Show("Пожалуйста, выберите товар для добавления в корзину.", "Ошибка", MessageBoxButtons.OK, MessageBoxIcon.Warning);
            }
        }

        private void cmbDiscountStrategy_SelectedIndexChanged(object sender, EventArgs e)
        {
            switch (cmbDiscountStrategy.SelectedIndex)
            {
                case 0:
                    cart.SetDiscountStrategy(new NoDiscountStrategy());
                    break;
                case 1:
                    cart.SetDiscountStrategy(new PercentageDiscountStrategy(15));
                    break;
                case 2:
                    cart.SetDiscountStrategy(new PromoCodeDiscountStrategy("TSUSHOP2025", 500));
                    break;
            }
            UpdateUI();
        }

        private void UpdateUI()
        {
            lblTotalAmount.Text = $"Общая сумма: {cart.GetTotalAmount():F2} рублей";
            lblDiscount.Text = $"Скидка: {cart.GetDiscountDescription()}";
            lblFinalTotal.Text = $"Итоговая сумма: {cart.GetFinalTotal():F2} рублей";
        }
    }
}