build: data/data.js

data/data.js: generate.scala \
		var/UnicodeData.txt
	bash ./docker/docker-run.sh sbt run

UNICODE_VERSION := 15.0.0

var/UnicodeData.txt:
	curl -Ssf -o var/UnicodeData.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/UnicodeData.txt'

var/NameAliases.txt:
	curl -Ssf -o var/NameAliases.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/NameAliases.txt'

