CC=clang
LD=ld
CL=sbcl
all:rw.c
	$(CC) -c ./rw.c
	$(LD) -shared -o ./rw.so ./rw.o
	@rm ./rw.o
	$(CL) --script ./compile.cl --no-userinit --no-sysinit --disable-debugger --end-toplevel-options
