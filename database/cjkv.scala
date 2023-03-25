
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
    } else {
      Nil;
    }
  }
}

