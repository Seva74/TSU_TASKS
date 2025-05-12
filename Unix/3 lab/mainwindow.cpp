#include "mainwindow.h"
#include <QVBoxLayout>
#include <QMessageBox>

MainWindow::MainWindow(QWidget *parent) : QMainWindow(parent) {
    QWidget *centralWidget = new QWidget(this);
    setCentralWidget(centralWidget);

    QVBoxLayout *layout = new QVBoxLayout(centralWidget);

    button = new QPushButton("Click me", this);
    checkbox = new QCheckBox("Check me", this);

    layout->addWidget(button);
    layout->addWidget(checkbox);

    connect(button, &QPushButton::clicked, this, &MainWindow::onButtonClicked);
}

void MainWindow::onButtonClicked() {
    if (checkbox->isChecked()) {
        QMessageBox::information(this, "CheckBox State", "The checkbox is checked.");
    } else {
        QMessageBox::information(this, "CheckBox State", "The checkbox is not checked.");
    }
}