build:
	dune clean
	dune build main.exe
	mv _build/default/main.exe c_compiler 

clean:
	dune clean
	rm -f c_compiler 
