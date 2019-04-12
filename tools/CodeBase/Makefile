CC = cc -g
all : nocontf90 deftab subinfo_calls wrfvar callgraph

nocontf90 : nocontf90.o util.o
	$(CC) -o $@ nocontf90.o util.o
	/bin/mv -f $@ ..

deftab : deftab.o util.o
	$(CC) -o $@ deftab.o util.o
	/bin/mv -f $@ ..

subinfo_calls : subinfo_calls.o util.o
	$(CC) -o $@ subinfo_calls.o util.o
	/bin/mv -f $@ ..

callgraph : callgraph.o util.o sym.o symtab_gen.o
	$(CC) -o $@ callgraph.o util.o sym.o symtab_gen.o
	/bin/mv -f $@ ..

wrfvar : wrfvar.o util.o
	$(CC) -o $@ wrfvar.o util.o
	/bin/mv -f $@ ..

clean :
	@/bin/rm -f *.o deftab nocontf90 callgraph subinfo_calls 
