import sys

def main():

    if len(sys.argv) < 3:
        print("usage: python write_rom.py <filename> <bytes>")
        exit(1)
    


    args = bytearray([int(x.replace("0x", ""), 16) for x in sys.argv[2:]])
    
    f = open(sys.argv[1], "wb");

    f.write(args)

if __name__ == "__main__":
    main()
