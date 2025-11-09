deploy:
	git checkout src
	./build rebuild
	git checkout master
	cp -R _site/* .
	rm -rf _site
	git add .
	git commit -C src
