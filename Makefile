ALL: sudoku-server

sudoku-server: server.o sudoku.o
	csc -o $@ $^

%.o: %.scm
	csc -c $<

clean:
	rm -f sudoku-server *.o
