
def combineHanguleSyllables(): Seq[(String, CodeInfo => CodeInfo)] = {
  // https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf
  val sbase = 0xAC00;
  val lbase = 0x1100;
  val vbase = 0x1161;
  val tbase = 0x11A7;
  val lcount = 19;
  val vcount = 21;
  val tcount = 28;
  val lcountAll = 95;
  val vcountAll = 71;
  val tcountAll = 89;
  val lnames = IndexedSeq("G", "GG", "N", "D", "DD", "R", "M", "B", "BB", "S", "SS", "", "J", "JJ", "C", "K", "T", "P", "H");
  val vnames = IndexedSeq("A", "AE", "YA", "YAE", "EO", "E", "YEO", "YE", "O", "WA", "WAE", "OE", "YO", "U", "WEO", "WE", "WI", "YU", "EU", "YI", "I");
  val tnames = IndexedSeq("", "G", "GG", "GS", "N", "NJ", "NH", "D", "L", "LG", "LM", "LB", "LS", "LT", "LP", "LH", "M", "B", "BS", "S", "SS", "NG", "J", "C", "K", "T", "P", "H");
  //val script = "Hangul";
  //val generalCategory = "Lo";
  //val bidiClass = "L";
  def generateSyllables(): Seq[(String, CodeInfo => CodeInfo)] = {
    (0 until lcount).flatMap { l =>
      (0 until vcount).flatMap { v =>
        (0 until tcount).flatMap { t =>
          val codePoint = sbase + l * (vcount * tcount) + v * tcount + t;
          val name = "HANGUL SYLLABLE " + lnames(l) + vnames(v) + tnames(t);
          val mappingCodes = codePointsToCode(if (t == 0) {
            Seq(lbase + l, vbase + v);
          } else {
            Seq(lbase + l, vbase + v, tbase + t);
          });
          Seq[(String, CodeInfo => CodeInfo)](
            (
              codePointToCode(codePoint),
              codeInfo => codeInfo.
                updateNameDefault(name).
                updateGeneralCategory("Lo").
                updateBidiClass("L").
                updateDecompositionType("canonical").
                updateDecompositionMapping(mappingCodes),
            ),
          );
        }
      }
    }
  }
  /*
  def generateCombining(): Seq[(String, CodeInfo => CodeInfo)] = {
    (0 until lcountAll).flatMap { l =>
      (0 until vcountAll).flatMap { v =>
        (0 until tcountAll).flatMap { t =>
          val code = codePointsToCode(if (t == 0) {
            Seq(lbase + l, vbase + v);
          } else {
            Seq(lbase + l, vbase + v, tbase + t);
          });
          val nameOpt = buildCombiningName(code, infoList);
          IndexedSeq(
            (code, CodeInfo.fromCombiningCharacterSequences(nameOpt)),
            (code, CodeInfo.fromScriptsEntry(script)),
          );
        }
      }
    }
  }
  */
  generateSyllables(); // ++ generateCombining();
}

