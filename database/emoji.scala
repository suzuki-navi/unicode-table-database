
def fetchEmojiData(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 2).flatMap { case (line, cols) =>
    val codePoints = cols(0).split("\\.\\.");
    val (rangeFirst, rangeList) = if (codePoints.length >= 2) {
      (Integer.parseInt(codePoints(0), 16), Integer.parseInt(codePoints(1), 16));
    } else {
      val c = Integer.parseInt(codePoints(0), 16);
      (c, c);
    }
    if (cols(1) == "Emoji_Presentation") {
      (rangeFirst to rangeList).flatMap { codePoint =>
        val code = codePointToCode(codePoint);
        if (codePoint >= 0x1F1E6 && codePoint <= 0x1F1FF) { // Regional Indicator
          val name = "RI " + (codePoint - 0x1F1E6 + 0x41).asInstanceOf[Char].toString;
          Seq(
            (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiPresentation()),
            (code, (codeInfo: CodeInfo) => codeInfo.updateNameAbbreviation(name)),
          );
        } else {
          Seq(
            (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiPresentation()),
            (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji")),
          );
        }
      }
    } else {
      Nil;
    }
  }
}

def fetchEmojiSequences(codePointInfoMap: Map[String, CodeInfo], path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 3).flatMap { case (line, cols) =>
    if (cols(1) == "Emoji_Keycap_Sequence") {
      val codePoints = cols(0).split(" ").map(c => Integer.parseInt(c, 16));
      val code = codePoints.map(c => codePointToCode(c)).mkString(" ");
      val name = cols(2);
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiPresentation()),
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameEmoji(name)),
      );
    } else if (cols(1) == "RGI_Emoji_Flag_Sequence") {
      val codePoints = cols(0).split(" ").map(c => Integer.parseInt(c, 16));
      val code = codePoints.map(c => codePointToCode(c)).mkString(" ");
      val nameRI = codePoints.map(c => (c - 0x1F1E6 + 0x41).asInstanceOf[Char].toString).mkString("");
      val name = cols(2);
      val nameCustom = name + " (" + nameRI + ")";
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiPresentation()),
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji-flag")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameEmoji(name)),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameCustom(Some(nameCustom))),
      );
    } else if (cols(1) == "RGI_Emoji_Tag_Sequence") {
      val codePoints = cols(0).split(" ").map(c => Integer.parseInt(c, 16));
      val code = codePoints.map(c => codePointToCode(c)).mkString(" ");
      val nameRI = codePoints.slice(1, codePoints.size - 1).map(c => (c - 0xE0020 + 0x20).asInstanceOf[Char].toString).mkString("");
      val name = cols(2);
      val nameCustom = name + " (" + nameRI + ")";
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiPresentation()),
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji-flag")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameEmoji(name)),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameCustom(Some(nameCustom))),
      );
    } else if (cols(1) == "RGI_Emoji_Modifier_Sequence") {
      val codePoints = cols(0).split(" ").map(c => Integer.parseInt(c, 16)).toSeq;
      val code = codePoints.map(c => codePointToCode(c)).mkString(" ");
      val nameOpt = buildCombiningName(codePoints, codePointInfoMap);
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiPresentation()),
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameCustom(nameOpt)),
      );
    } else {
      IndexedSeq.empty;
    }
  }
}

def fetchEmojiVariationSequences(codePointInfoMap: Map[String, CodeInfo], path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 2).flatMap { case (line, cols) =>
    val codePoints = cols(0).split(" ").map(c => Integer.parseInt(c, 16)).toSeq;
    val code = codePoints.map(c => codePointToCode(c)).mkString(" ");
    val baseCode = codePoints.slice(0, codePoints.length - 1);
    if (codePoints(codePoints.length - 1) == 0xFE0E) { // text VS
      val nameOpt = buildCombiningName(baseCode, codePointInfoMap).map(_ + "; text presentation selector");
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameCustom(nameOpt)),
      );
    } else if (codePoints(codePoints.length - 1) == 0xFE0F) { // emoji VS
      val nameOpt = buildCombiningName(baseCode, codePointInfoMap).map(_ + "; emoji presentation selector");
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiPresentation()),
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameCustom(nameOpt)),
      );
    } else {
      throw new Exception(line);
    }
  }
}


