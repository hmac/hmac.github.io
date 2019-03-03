deploy:
	git checkout src
	./build rebuild
	git checkout master
	cp -R _site/* .
	rm -rf _site
