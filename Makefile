
build: data/all.json

data/all.json: *.scala \
		var/UnicodeData.txt \
		var/BidiMirroring.txt \
		var/CaseFolding.txt \
		var/SpecialCasing.txt \
		var/NameAliases.txt \
		var/Scripts.txt \
		var/ScriptExtensions.txt \
		var/DerivedCoreProperties.txt \
		var/PropList.txt \
		var/DerivedNormalizationProps.txt \
		var/Blocks.txt \
		var/emoji-data.txt \
		var/emoji-variation-sequences.txt \
		var/emoji-sequences.txt \
		var/emoji-zwj-sequences.txt \
		var/emoji-test.txt \
		var/Unihan.zip \
		var/chise/IDS-UCS-Basic.txt \
		var/anytoold/bin/sbt
	var/anytoold/bin/sbt run

var/anytoold/bin/sbt:
	git clone https://github.com/suzuki-navi/anytoold.git var/anytoold

UNICODE_VERSION := 15.0.0
EMOJI_VERSION := 15.0

var/UnicodeData.txt:
	curl -Ssf -o var/UnicodeData.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/UnicodeData.txt'

var/BidiMirroring.txt:
	curl -Ssf -o var/BidiMirroring.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/BidiMirroring.txt'

var/CaseFolding.txt:
	curl -Ssf -o var/CaseFolding.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/CaseFolding.txt'

var/SpecialCasing.txt:
	curl -Ssf -o var/SpecialCasing.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/SpecialCasing.txt'

var/NameAliases.txt:
	curl -Ssf -o var/NameAliases.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/NameAliases.txt'

var/Scripts.txt:
	curl -Ssf -o var/Scripts.txt     'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/Scripts.txt'

var/ScriptExtensions.txt:
	curl -Ssf -o var/ScriptExtensions.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/ScriptExtensions.txt'

var/DerivedCoreProperties.txt:
	curl -Ssf -o var/DerivedCoreProperties.txt    'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/DerivedCoreProperties.txt'

var/PropList.txt:
	curl -Ssf -o var/PropList.txt    'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/PropList.txt'

var/DerivedNormalizationProps.txt:
	curl -Ssf -o var/DerivedNormalizationProps.txt    'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/DerivedNormalizationProps.txt'

var/Blocks.txt:
	curl -Ssf -o var/Blocks.txt      'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/Blocks.txt'

var/emoji-data.txt:
	curl -Ssf -o var/emoji-data.txt  'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/emoji/emoji-data.txt'

var/emoji-variation-sequences.txt:
	curl -Ssf -o var/emoji-variation-sequences.txt 'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/emoji/emoji-variation-sequences.txt'

var/emoji-sequences.txt:
	curl -Ssf -o var/emoji-sequences.txt 'https://www.unicode.org/Public/emoji/$(EMOJI_VERSION)/emoji-sequences.txt'

var/emoji-zwj-sequences.txt:
	curl -Ssf -o var/emoji-zwj-sequences.txt 'https://www.unicode.org/Public/emoji/$(EMOJI_VERSION)/emoji-zwj-sequences.txt'

var/emoji-test.txt:
	curl -Ssf -o var/emoji-test.txt 'https://www.unicode.org/Public/emoji/$(EMOJI_VERSION)/emoji-test.txt'

var/Unihan.zip:
	curl -Ssf -o var/Unihan.zip      'https://www.unicode.org/Public/$(UNICODE_VERSION)/ucd/Unihan.zip'
	cd var; unzip Unihan.zip

var/chise/IDS-UCS-Basic.txt:
	git clone https://gitlab.chise.org/CHISE/ids.git var/chise

