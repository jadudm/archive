all:
	raco exe archive.rkt
	raco distribute archive-dist archive
	tar cvzf archive-dist.tgz archive-dist

exe:
	raco exe archive.rkt

setup:
	# sudo add-apt-repository ppa:plt/racket
	sudo apt install par2 racket
	
install:
	mv archive ~/bin/
