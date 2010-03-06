.PHONY: all

all: index.html logo.png logo.html

index.html: README.markdown logo.include
	pandoc --standalone \
               --css=style.css \
               --smart \
               --sanitize-html \
               --table-of-contents \
               --email-obfuscation=references \
               --include-before-body=logo.include \
               --output=$@ $<

logo.png: logo.lhs
	runhaskell $<

logo.html: logo.lhs
	pandoc --standalone --output=$@ $<
