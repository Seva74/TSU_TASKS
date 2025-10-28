import argparse
import os
import sys
import tarfile
import time
from pathlib import Path
import bz2
import compression.zstd as zstd

# --------------------- АРГУМЕНТЫ ---------------------
def get_args():
    parser = argparse.ArgumentParser(
        description="Архиватор/разархиватор для zstd или bz2 файлов/папок с прогресс-баром",
        epilog="""
            Примеры:
            python archiver.py -c file.txt file.zst --benchmark
            python archiver.py -c myfolder data.tar.bz2 --progress
            python archiver.py -x archive.tar.zst output_folder
            python archiver.py -x file.bz2 restored.txt --benchmark
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument("-c", "--compress", action="store_true", help="Сжать")
    parser.add_argument("-x", "--extract", action="store_true", help="Распаковать")
    parser.add_argument("source", help="Что сжимаем или откуда распаковываем")
    parser.add_argument("dest", help="Куда сохраняем архив или распаковываем")
    parser.add_argument("-b", "--benchmark", action="store_true", help="Показать время и размер")
    parser.add_argument("-p", "--progress", action="store_true", help="Прогресс-бар")
    return parser.parse_args()

# --------------------- РАЗМЕР ФАЙЛА/ПАПКИ ---------------------
def get_size(path):
    p = Path(path)
    if p.is_file():
        return p.stat().st_size
    elif p.is_dir():
        return sum(f.stat().st_size for f in p.rglob('*') if f.is_file())
    return 0

# --------------------- ПРОГРЕСС-БАР ---------------------
def show_progress(done, total, label=""):
    if total == 0:
        return
    width = 30
    percent = done / total
    filled = int(width * percent)
    bar = "█" * filled + "░" * (width - filled)
    sys.stdout.write(f"\r{label} |{bar}| {percent*100:5.1f}%")
    sys.stdout.flush()
    if done >= total:
        print()

# ---------------- СЖАТИЕ ZSTD ---------------------
def compress_with_zstd(input_file, output_file, progress=False):
    data = Path(input_file).read_bytes()
    total = len(data)
    compressor = zstd.ZstdCompressor()
    
    compressed = bytearray()
    chunk = 1024 * 256
    for i in range(0, total, chunk):
        part = data[i:i+chunk]
        if progress:
            show_progress(i + len(part), total, "zstd: ")
        compressed.extend(compressor.compress(part))
    compressed.extend(compressor.flush())
    
    Path(output_file).write_bytes(compressed)
    if progress:
        print("Готово!")

# --------------------- РАСПАКОВКА ZSTD ---------------------
def extract_with_zstd(archive, output_file, progress=False):
    data = Path(archive).read_bytes()
    total = len(data)
    decompressor = zstd.ZstdDecompressor()
    
    result = bytearray()
    chunk = 1024 * 256
    i = 0
    while i < total:
        part = data[i:i+chunk]
        if progress:
            show_progress(i + len(part), total, "zstd: ")
        result.extend(decompressor.decompress(part))
        i += chunk
    
    Path(output_file).write_bytes(result)
    if progress:
        print("Распаковано!")

# --------------------- СЖАТИЕ BZ2 ---------------------
def compress_with_bz2(input_file, output_file, progress=False):
    data = Path(input_file).read_bytes()
    total = len(data)
    compressed = bz2.compress(data)
    Path(output_file).write_bytes(compressed)
    if progress:
        show_progress(total, total, "bz2: ")
        print("Готово!")

# --------------------- РАСПАКОВКА BZ2 ---------------------
def extract_with_bz2(archive, output_file, progress=False):
    data = Path(archive).read_bytes()
    total = len(data)
    decompressed = bz2.decompress(data)
    Path(output_file).write_bytes(decompressed)
    if progress:
        show_progress(total, total, "bz2: ")
        print("Распаковано!")

# --------------------- TAR ДЛЯ ПАПОК ---------------------
def make_tar(folder, tar_path):
    print(f"Создается tar из папки: {folder}")
    with tarfile.open(tar_path, "w") as tar:
        tar.add(folder, arcname=os.path.basename(folder))

def extract_tar(tar_path, dest_folder):
    os.makedirs(dest_folder, exist_ok=True)
    print(f"Распаковывается tar в: {dest_folder}")
    with tarfile.open(tar_path, "r") as tar:
        tar.extractall(dest_folder)

# --------------------- ОСНОВНАЯ ЛОГИКА ---------------------
def main():
    args = get_args()
    
    if (args.compress and args.extract) or (not args.compress and not args.extract):
        print("Ошибка: укажите ЛИБО -c (сжать) ЛИБО -x (распаковать)")
        sys.exit(1)

    src = Path(args.source)
    dst = Path(args.dest)
    start_time = time.time() if args.benchmark else None

    is_dir = src.is_dir()
    is_tar = len(dst.suffixes) == 2 and dst.suffixes[0] == '.tar'
    comp_ext = dst.suffix if args.compress else src.suffix

    if comp_ext not in ['.zst', '.bz2']:
        print("Ошибка: поддерживаются только .zst и .bz2 (или .tar.zst/.tar.bz2)")
        sys.exit(1)

    if args.compress:
        if is_dir:
            if not is_tar:
                print("Ошибка: для папки нужен вызодной файл .tar.zst или .tar.bz2")
                sys.exit(1)
            temp_tar = "temp_archive.tar"
            make_tar(src, temp_tar)
            input_file = temp_tar
        else:
            input_file = src

        if comp_ext == '.zst':
            compress_with_zstd(input_file, dst, args.progress)
        else:
            compress_with_bz2(input_file, dst, args.progress)

        if is_dir and os.path.exists("temp_archive.tar"):
            os.remove("temp_archive.tar")

    else:
        is_tar_src = len(src.suffixes) == 2 and src.suffixes[0] == '.tar'
        if is_tar_src:
            temp_tar = "temp_extracted.tar"
            if comp_ext == '.zst':
                extract_with_zstd(src, temp_tar, args.progress)
            else:
                extract_with_bz2(src, temp_tar, args.progress)
            extract_tar(temp_tar, dst)
            if os.path.exists(temp_tar):
                os.remove(temp_tar)
        else:
            if comp_ext == '.zst':
                extract_with_zstd(src, dst, args.progress)
            else:
                extract_with_bz2(src, dst, args.progress)

    if args.benchmark:
        elapsed = time.time() - start_time
        input_sz = get_size(args.source)
        output_sz = get_size(args.dest)
        print("\n" + "="*40)
        print("РЕЗУЛЬТАТЫ бенчмарка")
        print(f"Время:     {elapsed:.2f} сек")
        print(f"Вход:      {input_sz / 1024:.1f} КБ")
        print(f"Выход:     {output_sz / 1024:.1f} КБ")
        if input_sz > 0:
            ratio = output_sz / input_sz
            print(f"Сжатие:    {ratio:.2f}x")
        print("="*40)

if __name__ == "__main__":
    main()