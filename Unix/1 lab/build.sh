#!/bin/sh

if [ $# -ne 1 ]; then
    echo "Error: There must me only 1 argument in $0" >&2
    exit 1
fi

SOURCE_FILE="$1"

if [ ! -f "$SOURCE_FILE" ]; then
    echo "Error: File '$SOURCE_FILE' does not exist" >&2
    exit 2
fi

SOURCE_DIR=$(pwd)
TEMP_DIR=$(mktemp -d) || {
    echo "Error: Failed to create temp dir" >&2
    exit 3
}

clean_dir() {
    rm -rf "$TEMP_DIR"
    exit "$1"
}

trap 'clean_dir 130' INT
trap 'clean_dir 143' TERM
trap 'clean_dir $?' EXIT


case "$SOURCE_FILE" in
    *.c)
        COMPILER="cc"
        ;;
    *.tex)
        ;;
    *)
        echo "Error: Unsupported file type" >&2
        clean_dir 4
        ;;
esac

OUTPUT=$(grep '&Output:' "$SOURCE_FILE" | sed 's/.*&Output:\s*//')

if [ -z "$OUTPUT" ]; then
    echo "Error: No output filename specified with &Output:" >&2
    clean_dir 5
fi

cp "$SOURCE_FILE" "$TEMP_DIR/" || {
    echo "Error: Failed to copy" >&2
    clean_dir 6
}

cd "$TEMP_DIR" || {
    echo "Error: Failed to change directory" >&2
    clean_dir 7
}

SOURCE_NAME=$(basename "$SOURCE_FILE")

case "$SOURCE_FILE" in
    *.c)
        $COMPILER "$SOURCE_NAME" -o "$OUTPUT" 2>/tmp/err$$
        if [ $? -ne 0 ]; then
            cat /tmp/err$$ >&2
            rm -f /tmp/err$$
            clean_dir 8
        fi
        rm -f /tmp/err$$
        ;;
    *.tex)
        pdflatex "$SOURCE_NAME" >/dev/null 2>/tmp/err$$
        mv "${SOURCE_NAME%.tex}.pdf" "$OUTPUT"
        if [ $? -ne 0 ]; then
            rm -f /tmp/err$$
            clean_dir 10
        fi
        rm -f /tmp/err$$
        ;;
esac

if [ -f "$OUTPUT" ]; then
    echo "Output file created: $OUTPUT" >&2
else
    clean_dir 9
fi

mv "$OUTPUT" "$SOURCE_DIR/" || {
    clean_dir 11
}

clean_dir 0