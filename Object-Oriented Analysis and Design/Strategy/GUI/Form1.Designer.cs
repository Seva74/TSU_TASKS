namespace TSUShop
{
    partial class Form1
    {
        private System.ComponentModel.IContainer components = null;

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            this.lstAvailableItems = new System.Windows.Forms.ListBox();
            this.btnAddToCart = new System.Windows.Forms.Button();
            this.lstCartItems = new System.Windows.Forms.ListBox();
            this.lblTotalAmount = new System.Windows.Forms.Label();
            this.lblDiscount = new System.Windows.Forms.Label();
            this.lblFinalTotal = new System.Windows.Forms.Label();
            this.cmbDiscountStrategy = new System.Windows.Forms.ComboBox();
            this.lblSelectDiscount = new System.Windows.Forms.Label();
            this.lblAvailableItemsHeader = new System.Windows.Forms.Label();
            this.lblCartItemsHeader = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            // lstAvailableItems
            // 
            this.lstAvailableItems.FormattingEnabled = true;
            this.lstAvailableItems.ItemHeight = 16;
            this.lstAvailableItems.Location = new System.Drawing.Point(12, 40);
            this.lstAvailableItems.Name = "lstAvailableItems";
            this.lstAvailableItems.Size = new System.Drawing.Size(300, 200);
            this.lstAvailableItems.TabIndex = 0;
            // 
            // btnAddToCart
            // 
            this.btnAddToCart.Location = new System.Drawing.Point(12, 256);
            this.btnAddToCart.Name = "btnAddToCart";
            this.btnAddToCart.Size = new System.Drawing.Size(150, 35);
            this.btnAddToCart.TabIndex = 1;
            this.btnAddToCart.Text = "Добавить в корзину";
            this.btnAddToCart.UseVisualStyleBackColor = true;
            this.btnAddToCart.Click += new System.EventHandler(this.btnAddToCart_Click);
            // 
            // lstCartItems
            // 
            this.lstCartItems.FormattingEnabled = true;
            this.lstCartItems.ItemHeight = 16;
            this.lstCartItems.Location = new System.Drawing.Point(402, 40);
            this.lstCartItems.Name = "lstCartItems";
            this.lstCartItems.Size = new System.Drawing.Size(300, 200);
            this.lstCartItems.TabIndex = 2;
            // 
            // lblTotalAmount
            //18
            this.lblTotalAmount.AutoSize = true;
            this.lblTotalAmount.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.lblTotalAmount.Location = new System.Drawing.Point(402, 256);
            this.lblTotalAmount.Name = "lblTotalAmount";
            this.lblTotalAmount.Size = new System.Drawing.Size(180, 25);
            this.lblTotalAmount.TabIndex = 3;
            this.lblTotalAmount.Text = "Общая сумма: 0.00 рублей";
            // 
            // lblDiscount
            // 
            this.lblDiscount.AutoSize = true;
            this.lblDiscount.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.lblDiscount.Location = new System.Drawing.Point(402, 286);
            this.lblDiscount.Name = "lblDiscount";
            this.lblDiscount.Size = new System.Drawing.Size(150, 25);
            this.lblDiscount.TabIndex = 4;
            this.lblDiscount.Text = "Скидка: Без скидки";
            // 
            // lblFinalTotal
            // 
            this.lblFinalTotal.AutoSize = true;
            this.lblFinalTotal.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point);
            this.lblFinalTotal.Location = new System.Drawing.Point(402, 316);
            this.lblFinalTotal.Name = "lblFinalTotal";
            this.lblFinalTotal.Size = new System.Drawing.Size(200, 25);
            this.lblFinalTotal.TabIndex = 5;
            this.lblFinalTotal.Text = "Итоговая сумма: 0.00 рублей";
            // 
            // cmbDiscountStrategy
            // 
            this.cmbDiscountStrategy.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cmbDiscountStrategy.FormattingEnabled = true;
            this.cmbDiscountStrategy.Items.AddRange(new object[] {
                "Без скидки",
                "Скидка 15%",
                "Промокод TSUSHOP2025"});
            this.cmbDiscountStrategy.Location = new System.Drawing.Point(12, 316);
            this.cmbDiscountStrategy.Name = "cmbDiscountStrategy";
            this.cmbDiscountStrategy.Size = new System.Drawing.Size(200, 24);
            this.cmbDiscountStrategy.TabIndex = 6;
            this.cmbDiscountStrategy.SelectedIndexChanged += new System.EventHandler(this.cmbDiscountStrategy_SelectedIndexChanged);
            // 
            // lblSelectDiscount
            // 
            this.lblSelectDiscount.AutoSize = true;
            this.lblSelectDiscount.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.lblSelectDiscount.Location = new System.Drawing.Point(12, 296);
            this.lblSelectDiscount.Name = "lblSelectDiscount";
            this.lblSelectDiscount.Size = new System.Drawing.Size(180, 20);
            this.lblSelectDiscount.TabIndex = 7;
            this.lblSelectDiscount.Text = "Выберите стратегию скидки:";
            // 
            // lblAvailableItemsHeader
            // 
            this.lblAvailableItemsHeader.AutoSize = true;
            this.lblAvailableItemsHeader.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.lblAvailableItemsHeader.Location = new System.Drawing.Point(12, 20);
            this.lblAvailableItemsHeader.Name = "lblAvailableItemsHeader";
            this.lblAvailableItemsHeader.Size = new System.Drawing.Size(150, 20);
            this.lblAvailableItemsHeader.TabIndex = 8;
            this.lblAvailableItemsHeader.Text = "Доступные товары:";
            // 
            // lblCartItemsHeader
            // 
            this.lblCartItemsHeader.AutoSize = true;
            this.lblCartItemsHeader.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point);
            this.lblCartItemsHeader.Location = new System.Drawing.Point(402, 20);
            this.lblCartItemsHeader.Name = "lblCartItemsHeader";
            this.lblCartItemsHeader.Size = new System.Drawing.Size(80, 20);
            this.lblCartItemsHeader.TabIndex = 9;
            this.lblCartItemsHeader.Text = "Корзина:";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(8F, 16F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.LightBlue;
            this.ClientSize = new System.Drawing.Size(800, 450);
            this.Controls.Add(this.lblCartItemsHeader);
            this.Controls.Add(this.lblAvailableItemsHeader);
            this.Controls.Add(this.lblSelectDiscount);
            this.Controls.Add(this.cmbDiscountStrategy);
            this.Controls.Add(this.lblFinalTotal);
            this.Controls.Add(this.lblDiscount);
            this.Controls.Add(this.lblTotalAmount);
            this.Controls.Add(this.lstCartItems);
            this.Controls.Add(this.btnAddToCart);
            this.Controls.Add(this.lstAvailableItems);
            this.Name = "Form1";
            this.Text = "TSUShop";
            this.Load += new System.EventHandler(this.Form1_Load);
            this.ResumeLayout(false);
            this.PerformLayout();
        }

        private System.Windows.Forms.ListBox lstAvailableItems;
        private System.Windows.Forms.Button btnAddToCart;
        private System.Windows.Forms.ListBox lstCartItems;
        private System.Windows.Forms.Label lblTotalAmount;
        private System.Windows.Forms.Label lblDiscount;
        private System.Windows.Forms.Label lblFinalTotal;
        private System.Windows.Forms.ComboBox cmbDiscountStrategy;
        private System.Windows.Forms.Label lblSelectDiscount;
        private System.Windows.Forms.Label lblAvailableItemsHeader;
        private System.Windows.Forms.Label lblCartItemsHeader;
    }
}