
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
            (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emojiFont")),
          );
        }
      }
    } else if (cols(1) == "Emoji_Modifier_Base") {
      // Some characters have Emoji_Modifier_Base but not Emoji_Presentation
      // ex) 26F9
      (rangeFirst to rangeList).flatMap { codePoint =>
        val code = codePointToCode(codePoint);
        Seq(
          (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiModifierBase()),
          // (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emojiFont")),
        );
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
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emojiFont")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameEmoji(name)),
      );
    } else if (cols(1) == "RGI_Emoji_Flag_Sequence") {
      val codePoints = cols(0).split(" ").map(c => Integer.parseInt(c, 16));
      val code = codePoints.map(c => codePointToCode(c)).mkString(" ");
      val nameRI = codePoints.map(c => (c - 0x1F1E6 + 0x41).asInstanceOf[Char].toString).mkString("");
      val name = cols(2);
      val nameCustom = name + " (" + nameRI + ")";
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emojiFont")),
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
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emojiFont")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji-flag")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameEmoji(name)),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameCustom(Some(nameCustom))),
      );
    } else if (cols(1) == "RGI_Emoji_Modifier_Sequence") {
      val codePoints = cols(0).split(" ").map(c => Integer.parseInt(c, 16)).toSeq;
      val code = codePoints.map(c => codePointToCode(c)).mkString(" ");
      val nameOpt = buildCombiningName(codePoints, codePointInfoMap);
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emojiFont")),
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
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emojiFont")),
        (code, (codeInfo: CodeInfo) => codeInfo.updateNameCustom(nameOpt)),
      );
    } else {
      throw new Exception(line);
    }
  }
}

def fetchEmojiTest(codePointInfoMap: Map[String, CodeInfo], path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  val GroupPattern = "\\A# group: (.+)\\z".r;
  val SubgroupPattern = "\\A# subgroup: (.+)\\z".r;
  usingDataFile3(path, 2).foldLeft[(String, String, List[(String, CodeInfo => CodeInfo)])]("", "", Nil) { (t1, t2) =>
    val (groupName, subgroupName, result) = t1;
    t2 match {
      case (GroupPattern(name), None) =>
        (name, "", result);
      case (SubgroupPattern(name), None) =>
        (groupName, name, result);
      case (_, Some(cols)) =>
        if (cols(1) == "fully-qualified" && groupName != "") {
          val codePoints = cols(0).split(" ").map(c => Integer.parseInt(c, 16)).toSeq;
          val code = codePoints.map(c => codePointToCode(c)).mkString(" ");
          (groupName, subgroupName,
            (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiGroup(groupName).updateEmojiSubgroup(subgroupName)) :: result);
        } else {
          (groupName, subgroupName, result);
        }
      case _ =>
        (groupName, subgroupName, result);
    }
  }._3.reverse;
}

def selectEmojiCharacters(codePointInfoMap: Map[String, CodeInfo]): Seq[(String, CodeInfo => CodeInfo)] = {
  codePointInfoMap.toSeq.flatMap { case (code, info) =>
    def isEmojiPresentation(info: CodeInfo): Boolean = {
      //info.emojiPresentation == Some(true) || info.emojiModifierBase == Some(true);
      info.emojiPresentation == Some(true);
    }
    def codeToHtml(code: String): String = {
      code.split(" ").map(Integer.parseInt(_, 16)).map(i => "&#x" + Integer.toString(i, 16).toUpperCase + ";").mkString("");
    }
    if (info.option.getOrElse(Nil).contains("emojiFont")) {
      if (code.endsWith(" FE0F")) {
        val parentCode = code.substring(0, code.length - 5);
        if (isEmojiPresentation(codePointInfoMap(parentCode))) {
          val html = codeToHtml(code);
          IndexedSeq(
            (parentCode, (codeInfo: CodeInfo) => codeInfo.updateHtml(html)),
          );
        } else if (codePointInfoMap.contains(code + " 20E3")) {
          IndexedSeq.empty;
        } else {
          IndexedSeq(
            (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji")),
          );
        }
      } else {
        IndexedSeq(
          (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji")),
        );
      }
    } else {
      if (code.endsWith(" FE0E")) {
        val parentCode = code.substring(0, code.length - 5);
        if (parentCode.length == 4 && Integer.parseInt(parentCode, 16) <= 0x7F) {
          IndexedSeq.empty;
        } else if (!isEmojiPresentation(codePointInfoMap(parentCode))) {
          val html = codeToHtml(code);
          IndexedSeq(
            (parentCode, (codeInfo: CodeInfo) => codeInfo.updateHtml(html)),
          );
        } else {
          IndexedSeq.empty;
        }
      } else {
        IndexedSeq.empty;
      }
    }
  } ++
  codePointInfoMap.toSeq.flatMap { case (code, info) =>
    if (info.emojiGroup == Some("Flags") && !info.option.getOrElse(Nil).contains("emoji-flag")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji-otherflag")),
      );
    } else if (info.option.getOrElse(Nil).contains("emoji") && info.emojiGroup == None) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiGroup("unknown")),
      );
    } else {
      IndexedSeq.empty;
    }
  }
}
