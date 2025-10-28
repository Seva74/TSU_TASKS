Лабораторная работа: Архиватор на Python 3.14
Автор: Викторов Всеволод, 932205

Архиватор/разархиватор для zstd или bz2 файлов/папок с прогресс-баром

Параметры:
Обязательные:
  source                Что сжимаем или откуда распаковываем
  dest                  Куда сохраняем архив или распаковываем

Опциональные:
  -h, --help            show this help message and exit
  -c, --compress        Сжать
  -x, --extract         Распаковать
  -b, --benchmark       Показать время и размер
  -p, --progress        Прогресс-бар

Примеры использования:
    python archiver.py -c -p -b file.txt file.zst --benchmark
    python archiver.py -c -p -b fmyfolder data.tar.bz2 --progress
    python archiver.py -x -p -b farchive.tar.zst output_folder
    python archiver.py -x -p -b ffile.bz2 restored.txt --benchmark