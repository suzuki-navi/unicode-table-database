build: data/all.json

data/all.json: generate.scala \
		var/UnicodeData.txt \
		var/NameAliases.txt \
		var/Scripts.txt \
		var/Blocks.txt \
		var/emoji-data.txt \
		var/Unihan.zip
	bash ../etc/scala-docker/docker-run.sh sbt run

UNICODE_VERSION := 15.0.0

var/UnicodeData.txt:
	curl -Ssf -o var/UnicodeData.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/UnicodeData.txt'

var/NameAliases.txt:
	curl -Ssf -o var/NameAliases.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/NameAliases.txt'

var/Scripts.txt:
	curl -Ssf -o var/Scripts.txt     'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/Scripts.txt'

var/Blocks.txt:
	curl -Ssf -o var/Blocks.txt      'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/Blocks.txt'

var/emoji-data.txt:
	curl -Ssf -o var/emoji-data.txt  'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/emoji/emoji-data.txt'

var/Unihan.zip:
	curl -Ssf -o var/Unihan.zip      'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/Unihan.zip'
	cd var; unzip Unihan.zip

