FROM ubuntu:latest

# Установка компилятора C++
RUN apt-get update \
    && apt-get install -y g++

# Создание рабочей директории и копирование cpp файла
RUN mkdir -p /usr/src/myapp
WORKDIR /usr/src/myapp
COPY HelloWorld.cpp /usr/src/myapp/

# Компиляция и запуск приложения при запуске контейнера
CMD g++ -o HelloWorld HelloWorld.cpp && ./HelloWorld
