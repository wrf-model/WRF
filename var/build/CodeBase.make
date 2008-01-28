nocontf90 : nocontf90.o util.o
	$(CC) -o $@ nocontf90.o util.o
	$(MV) -f $@ ..

deftab : deftab.o util.o
	$(CC) -o $@ deftab.o util.o
	$(MV) -f $@ ..

subinfo_calls : subinfo_calls.o util.o
	$(CC) -o $@ subinfo_calls.o util.o
	$(MV) -f $@ ..

callgraph : callgraph.o util.o sym.o symtab_gen.o
	$(CC) -o $@ callgraph.o util.o sym.o symtab_gen.o
	$(MV) -f $@ ..
