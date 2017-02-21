doc:
	scribble  ++xref-in setup/xref load-collections-xref --redirect-main http://docs.racket-lang.org/   --dest-name index.html manual.scrbl

clean:
	git clean -fdx .

web: clean plt doc
	scp -r * hashcollision.org:webapps/htdocs/ragg/


plt:
	raco pack --collect ragg.plt ragg
