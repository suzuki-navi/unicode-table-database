
def fetchUnihanVariants(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile2(path, 3).filter { case (line, cols) =>
    cols(0).startsWith("U+");
  }.flatMap { case (line, cols) =>
    val code = cols(0).substring(2);
    val fieldName = cols(1);
    val values: Seq[String] = cols(2).split(" ").toSeq.map(_.trim).map { s =>
      val p = s.indexOf("<");
      if (p >= 0) {
        s.substring(0, p);
      } else {
        s;
      }
    }.filter(_.startsWith("U+")).map(_.substring(2));
    values.map { v =>
      (code, (codeInfo: CodeInfo) =>
        fieldName match {
          case "kSemanticVariant" => codeInfo.updateCjkSemanticVariant(v);
          case "kSimplifiedVariant" => codeInfo.updateCjkSimplifiedVariant(v);
          case "kSpecializedSemanticVariant" => codeInfo.updateCjkSpecializedSemanticVariant(v);
          case "kSpoofingVariant" => codeInfo.updateCjkSpoofingVariant(v);
          case "kTraditionalVariant" => codeInfo.updateCjkTraditionalVariant(v);
          case "kZVariant" => codeInfo.updateCjkZVariant(v);
        }
      );
    }
  }
}

def fetchUnihanReadings(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile2(path, 3).filter { case (line, cols) =>
    cols(0).startsWith("U+");
  }.flatMap { case (line, cols) =>
    val code = cols(0).substring(2);
    val category = cols(1);
    if (category == "kDefinition") {
      val meaning = cols(2);
      Seq((code, (codeInfo: CodeInfo) => codeInfo.updateMeaning(meaning)));
    } else if (category == "kMandarin") {
      val readings = cols(2).split(" ").map(_.trim).filter(_.length > 0);
      readings.map { reading =>
        (code, (codeInfo: CodeInfo) => codeInfo.updateMandarinReading(reading));
      }
    } else if (category == "kCantonese") {
      val readings = cols(2).split(" ").map(_.trim).filter(_.length > 0);
      readings.map { reading =>
        (code, (codeInfo: CodeInfo) => codeInfo.updateCantoneseReading(reading));
      }
    } else if (category == "kHangul") {
      val readings = cols(2).split(" ").map(_.trim).filter(_.length > 0);
      readings.flatMap { r =>
        val reading = {
          val p = r.indexOf(":");
          if (p >= 0) {
            r.substring(0, p);
          } else {
            r;
          }
        }
        if (reading.length == 1) {
          val hangulCode = codePointToCode(reading(0));
          val hanja = code;
          IndexedSeq(
            (code, (codeInfo: CodeInfo) => codeInfo.updateKoreanReading(reading)),
            (hangulCode, (codeInfo: CodeInfo) => codeInfo.updateKoreanHanja(hanja)),
          );
        } else {
          IndexedSeq(
            (code, (codeInfo: CodeInfo) => codeInfo.updateKoreanReading(reading)),
          );
        }
      }
    } else if (category == "kJapaneseKun") {
      val readings = cols(2).split(" ").map(_.trim).filter(_.length > 0).map(s => romajiToKana(s, false));
      readings.map { reading =>
        (code, (codeInfo: CodeInfo) => codeInfo.updateJapaneseKunReading(reading));
      }
    } else if (category == "kJapaneseOn") {
      val readings = cols(2).split(" ").map(_.trim).filter(_.length > 0).map(s => romajiToKana(s, true));
      readings.map { reading =>
        (code, (codeInfo: CodeInfo) => codeInfo.updateJapaneseOnReading(reading));
      }
    } else {
      Nil;
    }
  }
}

def romajiToKana(romaji: String, isKatakana: Boolean): String = {
  val table: Seq[(String, String)] = Seq(
    ("A", "あ"),
    ("I", "い"),
    ("U", "う"),
    ("E", "え"),
    ("O", "お"),
    ("KA", "か"),
    ("KI", "き"),
    ("KU", "く"),
    ("KE", "け"),
    ("KO", "こ"),
    ("SA", "さ"),
    ("SHI", "し"),
    ("SU", "す"),
    ("SE", "せ"),
    ("SO", "そ"),
    ("TA", "た"),
    ("CHI", "ち"),
    ("TSU", "つ"),
    ("TE", "て"),
    ("TO", "と"),
    ("NA", "な"),
    ("NI", "に"),
    ("NU", "ぬ"),
    ("NE", "ね"),
    ("NO", "の"),
    ("HA", "は"),
    ("HI", "ひ"),
    ("HU", "ふ"),
    ("FU", "ふ"),
    ("HE", "へ"),
    ("HO", "ほ"),
    ("MA", "ま"),
    ("MI", "み"),
    ("MU", "む"),
    ("ME", "め"),
    ("MO", "も"),
    ("YA", "や"),
    ("YU", "ゆ"),
    ("YO", "よ"),
    ("RA", "ら"),
    ("RI", "り"),
    ("RU", "る"),
    ("RE", "れ"),
    ("RO", "ろ"),
    ("WA", "わ"),
    ("WO", "を"),
    ("GA", "が"),
    ("GI", "ぎ"),
    ("GU", "ぐ"),
    ("GE", "げ"),
    ("GO", "ご"),
    ("ZA", "ざ"),
    ("JI", "じ"),
    ("ZU", "ず"),
    ("ZE", "ぜ"),
    ("ZO", "ぞ"),
    ("DA", "だ"),
    ("DI", "ぢ"),
    ("DU", "づ"),
    ("DE", "で"),
    ("DO", "ど"),
    ("BA", "ば"),
    ("BI", "び"),
    ("BU", "ぶ"),
    ("BE", "べ"),
    ("BO", "ぼ"),
    ("PA", "ぱ"),
    ("PI", "ぴ"),
    ("PU", "ぷ"),
    ("PE", "ぺ"),
    ("PO", "ぽ"),
    ("KYA", "きゃ"),
    ("KYU", "きゅ"),
    ("KYO", "きょ"),
    ("SHA", "しゃ"),
    ("SHU", "しゅ"),
    ("SHYU", "しゅ"),
    ("SHO", "しょ"),
    ("SHYO", "しょ"),
    ("CHA", "ちゃ"),
    ("CHU", "ちゅ"),
    ("CHYU", "ちゅ"),
    ("CHO", "ちょ"),
    ("HYA", "ひゃ"),
    ("HYU", "ひゅ"),
    ("HYO", "ひょ"),
    ("MYA", "みゃ"),
    ("MYU", "みゅ"),
    ("MYO", "みょ"),
    ("RYA", "りゃ"),
    ("RYU", "りゅ"),
    ("RYO", "りょ"),
    ("GYA", "ぎゃ"),
    ("GYU", "ぎゅ"),
    ("GYO", "ぎょ"),
    ("JA", "じゃ"),
    ("JU", "じゅ"),
    ("JO", "じょ"),
    ("BYA", "びゃ"),
    ("BYU", "びゅ"),
    ("BYO", "びょ"),
    ("PYA", "ぴゃ"),
    ("PYU", "ぴゅ"),
    ("PYO", "ぴょ"),
    ("FI", "ふぃ"),
    ("N", "ん"),
  );
  def sub(index: Int, result: String): String = {
    if (index >= romaji.length) {
      result;
    } else {
      table.find { case (r, k) =>
        romaji.startsWith(r, index);
      } match {
        case Some((r, k)) =>
          sub(index + r.length, result + k);
        case None =>
          if (index + 2 <= romaji.length && romaji.substring(index, index + 1) == romaji.substring(index + 1, index + 2)) {
            sub(index + 1, result + "っ");
          } else {
            println("Unknown romaji: " + romaji);
            romaji;
            //throw new Exception("Unknown romaji: " + romaji.substring(index));
            //sub(index + 1, result + romaji.substring(index, index + 1));
          }
      }
    }
  }
  if (isKatakana) {
    sub(0, "").map { ch =>
      if (ch <= 0x80) {
        ch;
      } else {
        (ch + 96).toChar;
      }
    }
  } else {
    sub(0, "");
  }
}

def selectCJKIdeographName(codePointInfoMap: Map[String, CodeInfo]): Seq[(String, CodeInfo => CodeInfo)] = {
  codePointInfoMap.toSeq.flatMap { case (code, info) =>
    val block = info.block.getOrElse("");
    if (block.startsWith("CJK Unified Ideographs") ||
        block == "CJK Compatibility Ideographs" ||
        block == "CJK Compatibility Supplement") {
      val name = if (info.decompositionMapping.isDefined) {
        "CJK COMPATIBILITY IDEOGRAPH-" + code;
      } else {
        "CJK UNIFIED IDEOGRAPH-" + code;
      }
      // https://www.unicode.org/reports/tr38/tr38-33.html#BlockListing
      Seq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameCustom(Some(name))),
        (code, (codeInfo: CodeInfo) => codeInfo.updateUnihanFlag(true)),
      );
    } else {
      Nil;
    }
  }
}
