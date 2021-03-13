FILE="$1"
FNAME="${FILE%%.*}"
nasm -f elf64 "$1" && ld -s -o "$2" "$FNAME.o"
rm "$FNAME.o"
