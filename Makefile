build:
	stack build --copy-bins

run:
	stack build --copy-bins
	hastrew-exe

server:
	stack build --copy-bins
	hastrew-exe --server
