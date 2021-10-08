all: records.html

%.html: %.pdc
	pandoc -s -r markdown --mathjax $< -o $@
