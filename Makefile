index.html: README.markdown logo.html
	pandoc --standalone \
               --css=style.css \
               --smart \
               --sanitize-html \
               --table-of-contents \
               --email-obfuscation=references \
               --include-before-body=logo.html \
               --output=$@ $<

logo.png: logo.hs
	runhaskell $<
