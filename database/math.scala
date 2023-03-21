
def selectMathematicalSymbols(codePointInfoMap: Map[String, CodeInfo]): Seq[(String, CodeInfo => CodeInfo)] = {
  codePointInfoMap.toSeq.flatMap { case (code, info) =>
    val codePoints = code.split(" ").map(c => Integer.parseInt(c, 16)).toSeq;
    val codePoint = codePoints(0);
    if (info.generalCategory == Some("Sm")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("math")),
      );
    } else if (info.block == Some("Mathematical Alphanumeric Symbols")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("math")),
      );
    } else if (info.block == Some("Arabic Mathematical Alphabetic Symbols")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("math")),
      );
    } else if (info.block == Some("Mathematical Operators")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("math")),
      );
    } else if (info.block == Some("Supplemental Mathematical Operators")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("math")),
      );
    } else if (info.block == Some("Miscellaneous Mathematical Symbols-A")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("math")),
      );
    } else if (info.block == Some("Miscellaneous Mathematical Symbols-B")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("math")),
      );
    } else if (codePoint >= 0x2061 && codePoint <= 0x2064 ||
               codePoint >= 0x210A && codePoint <= 0x2115 ||
               codePoint >= 0x2118 && codePoint <= 0x211D ||
               codePoint >= 0x2123 && codePoint <= 0x2134 ||
               codePoint >= 0x213C && codePoint <= 0x2149 ||
               codePoint >= 0x2308 && codePoint <= 0x230B ||
               codePoint >= 0x2320 && codePoint <= 0x2321 ||
               codePoint >= 0x239B && codePoint <= 0x23B6) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("math")),
      );
    } else {
      IndexedSeq.empty;
    }
  }
}

def selectArrowSymbols(codePointInfoMap: Map[String, CodeInfo]): Seq[(String, CodeInfo => CodeInfo)] = {
  codePointInfoMap.toSeq.flatMap { case (code, info) =>
    val codePoints = code.split(" ").map(c => Integer.parseInt(c, 16)).toSeq;
    val codePoint = codePoints(0);
    if (info.block == Some("Arrows")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("arrow")),
      );
    } else if (info.block == Some("Supplemental Arrows-A")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("arrow")),
      );
    } else if (info.block == Some("Supplemental Arrows-B")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("arrow")),
      );
    } else if (info.block == Some("Supplemental Arrows-C")) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("arrow")),
      );
    } else if (codePoint >= 0x2303 && codePoint <= 0x2304) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("arrow")),
      );
    } else if (codePoint >= 0x2B00 && codePoint <= 0x2B11 ||
               codePoint >= 0x2B30 && codePoint <= 0x2B4D ||
               codePoint >= 0x2B60 && codePoint <= 0x2B73 ||
               codePoint >= 0x2B76 && codePoint <= 0x2B95 ||
               codePoint >= 0x2B98 && codePoint <= 0x2BB9 ||
               codePoint >= 0x2BEC && codePoint <= 0x2BEF) {
      IndexedSeq(
        (code, (codeInfo: CodeInfo) => codeInfo.updateOption("arrow")),
      );
    } else {
      IndexedSeq.empty;
    }
  }
}

