wehr_sommercampus2018.html: talk.txt Makefile
	pandoc --slide-level 2 --highlight-style=pygments -t slidy -s -f markdown -o $@ talk.txt

pdf: wehr_sommercampus2018.pdf

wehr_sommercampus2018.pdf: talk.txt header.tex body-header.tex
	pandoc --slide-level 2 -s -f markdown -t beamer -H header.tex -B body-header.tex -o $@ $<
