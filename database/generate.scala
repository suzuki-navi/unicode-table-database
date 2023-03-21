import java.io.PrintWriter;

import scala.collection.immutable.SeqMap;
import scala.io.Source;
import scala.util.Using;

import io.circe.Encoder;
import io.circe.syntax._;
import io.circe.generic.semiauto.deriveEncoder;

@main def main: Unit = {
  val codePointInfoMap: Map[String, CodeInfo] = {
    val codePointInfoMap: Map[String, CodeInfo] = (
      fetchUnicodeData("var/UnicodeData.txt") ++
      fetchBidiMirroring("var/BidiMirroring.txt") ++
      fetchNameAliases("var/NameAliases.txt") ++
      fetchScripts("var/Scripts.txt") ++
      fetchEmojiData("var/emoji-data.txt") ++
      fetchUnihanReadings("var/Unihan_Readings.txt") ++
      Nil
    ).foldLeft(Map.empty) { (infoMap, entry) =>
      val (code, updator) = entry;
      CodeInfo.updated(infoMap, code)(updator);
    }

    val codePointInfoMap2: Map[String, CodeInfo] = (
      fetchBlocks(codePointInfoMap, "var/Blocks.txt")
    ).foldLeft(codePointInfoMap) { (infoMap, entry) =>
      val (code, updator) = entry;
      CodeInfo.updated(infoMap, code)(updator);
    }

    (
      fetchEmojiSequences(codePointInfoMap2, "var/emoji-sequences.txt") ++
      fetchEmojiVariationSequences(codePointInfoMap2, "var/emoji-variation-sequences.txt") ++
      fetchEmojiZwjSequences(codePointInfoMap2, "var/emoji-zwj-sequences.txt") ++
      Nil
    ).foldLeft(codePointInfoMap2) { (infoMap, entry) =>
      val (code, updator) = entry;
      CodeInfo.updated(infoMap, code)(updator);
    }
  }

  val infoMap: Map[String, CodeInfo] = codePointInfoMap;

  output(infoMap, "data/all.json");
}

def fetchUnicodeData(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 15).filter { case (line, cols) =>
    val name = cols(1);
    !(name.startsWith("<") && name != "<control>");
  }.flatMap { case (line, cols) =>
    val code = cols(0);
    val name = if (cols(1) == "<control>") None else Some(cols(1));
    val generalCategory = cols(2);
    val bidiClass = cols(4);
    Seq[(String, CodeInfo => CodeInfo)](
      (code, codeInfo => codeInfo.updateGeneralCategory(generalCategory)),
      (code, codeInfo => codeInfo.updateBidiClass(bidiClass)),
    ) ++ name.map(name => (code, (codeInfo: CodeInfo) => {
      codeInfo.updateNameDefault(name);
    }));
  }
}

def fetchBidiMirroring(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 2).flatMap { case (line, cols) =>
    val code1 = cols(0);
    val code2 = cols(1);
    Seq[(String, CodeInfo => CodeInfo)](
      (code1, codeInfo => codeInfo.updateBidiMirroring(code2)),
    );
  }
}

def fetchNameAliases(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 3).flatMap { case (line, cols) =>
    val code = cols(0);
    val name = cols(1);
    cols(2) match {
      case "correction" =>
        Seq((code, (codeInfo: CodeInfo) => codeInfo.updateNameCorrection(name)));
      case "control" =>
        Seq((code, (codeInfo: CodeInfo) => codeInfo.appendNameControl(name)));
      case "alternate" =>
        Seq((code, (codeInfo: CodeInfo) => codeInfo.updateNameAlternate(name)));
      case "figment" =>
        Seq((code, (codeInfo: CodeInfo) => codeInfo.updateNameFigment(name)));
      case "abbreviation" =>
        Seq((code, (codeInfo: CodeInfo) => codeInfo.updateNameAbbreviation(name)));
      case v =>
        throw new Exception(v);
    }
  }
}

def fetchScripts(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 2).flatMap { case (line, cols) =>
    val codePoints = cols(0).split("\\.\\.");
    val (rangeFirst, rangeList) = if (codePoints.length >= 2) {
      (Integer.parseInt(codePoints(0), 16), Integer.parseInt(codePoints(1), 16));
    } else {
      val c = Integer.parseInt(codePoints(0), 16);
      (c, c);
    }
    val scriptName = cols(1);
    (rangeFirst to rangeList).map { c =>
      (codePointToCode(c), (codeInfo: CodeInfo) => codeInfo.updateScript(scriptName));
    }
  }
}

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

def fetchEmojiZwjSequences(codePointInfoMap: Map[String, CodeInfo], path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 3).flatMap { case (line, cols) =>
    val codePoints = cols(0).split(" ").map(c => Integer.parseInt(c, 16)).toSeq;
    val code = codePoints.map(c => codePointToCode(c)).mkString(" ");
    val name = cols(2);
    IndexedSeq(
      (code, (codeInfo: CodeInfo) => codeInfo.updateEmojiPresentation()),
      (code, (codeInfo: CodeInfo) => codeInfo.updateOption("emoji")),
      (code, (codeInfo: CodeInfo) => codeInfo.updateNameEmoji(name)),
    );
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
      readings.map { r =>
        val reading = {
          val p = r.indexOf(":");
          if (p >= 0) {
            r.substring(0, p);
          } else {
            r;
          }
        }
        (code, (codeInfo: CodeInfo) => codeInfo.updateKoreanReading(reading));
      }
    } else {
      Nil;
    }
  }
}

def fetchBlocks(codePointInfoMap: Map[String, CodeInfo], path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 2).flatMap { case (line, cols) =>
    val codePoints = cols(0).split("\\.\\.");
    val (rangeFirst, rangeList) = if (codePoints.length >= 2) {
      (Integer.parseInt(codePoints(0), 16), Integer.parseInt(codePoints(1), 16));
    } else {
      val c = Integer.parseInt(codePoints(0), 16);
      (c, c);
    }
    val blockName = cols(1);
    (rangeFirst to rangeList).flatMap { c =>
      val code = codePointToCode(c);
      if (codePointInfoMap.contains(code)) {
        Some((code, (codeInfo: CodeInfo) => codeInfo.updateBlock(blockName)));
      } else {
        None;
      }
    }
  }.toSeq;
}

def usingDataFile(path: String, colCount: Int): Seq[(String, Seq[String])] = {
  println(path);
  Using(Source.fromFile(path)) { source =>
    source.getLines().flatMap { line =>
      val p = line.indexOf("#");
      val line2 = if (p < 0) {
        line;
      } else {
        line.substring(0, p);
      }
      if (line2 == "") {
        None;
      } else {
        val cols = line2.split(";", colCount).toIndexedSeq.map(_.trim);
        if (cols.size != colCount) {
          throw new Exception(line);
        }
        Some((line, cols));
      }
    }.toSeq;
  }.get;
}

def usingDataFile2(path: String, colCount: Int): Seq[(String, Seq[String])] = {
  Using(Source.fromFile(path)) { source =>
    source.getLines().flatMap { line =>
      val p = line.indexOf("#");
      val line2 = if (p < 0) {
        line;
      } else {
        line.substring(0, p);
      }
      if (line2 == "") {
        None;
      } else {
        val cols = line2.split("\t", colCount).toIndexedSeq.map(_.trim);
        if (cols.size != colCount) {
          throw new Exception(line);
        }
        Some((line, cols));
      }
    }.toSeq;
  }.get;
}

def buildCombiningName(codePoints: Seq[Int], codePointInfoMap: Map[String, CodeInfo]): Option[String] = {
  val nameOpts: Seq[Option[String]] = codePoints.map { codePoint =>
    codePointInfoMap.get(codePointToCode(codePoint)).flatMap(info => CodeInfoExtra.nameFromInfo(info));
  }
  if (nameOpts.exists(_.isEmpty)) {
    None;
  } else {
    Some(nameOpts.map(_.get).mkString("; "));
  }
}

def codePointToCode(codePoint: Int): String = {
  val s = Integer.toString(codePoint, 16).toUpperCase;
  if (s.length <= 3) {
    ("0000" + s).substring(s.length);
  } else {
    s;
  }
}

case class CodeInfo(
  nameDefault: Option[String],

  // https://www.unicode.org/Public/15.0.0/ucd/NameAliases.txt
  nameCorrection: Option[String],
  nameControl: Option[Seq[String]],
  nameAlternate: Option[String],
  nameFigment: Option[String],
  nameAbbreviation: Option[Seq[String]],
  nameEmoji: Option[String],
  nameCustom: Option[String],

  // https://www.unicode.org/reports/tr44/tr44-30.html#General_Category_Values
  generalCategory: Option[String],

  block: Option[String],

  script: Option[String],

  // https://www.unicode.org/reports/tr44/tr44-30.html#Bidi_Class_Values
  bidiClass: Option[String],

  bidiMirroring: Option[String],

  emojiPresentation: Option[Boolean],

  meaning: Option[String],
  mandarinReading: Option[Seq[String]],
  cantoneseReading: Option[String],
  koreanReading: Option[Seq[String]],

  option: Option[Seq[String]],

) {

  def updateNameDefault(newValue: String) = this.copy(nameDefault = mergeValue(nameDefault, newValue));
  def updateNameCorrection(newValue: String) = this.copy(nameCorrection = mergeValue(nameCorrection, newValue));
  def appendNameControl(newValue: String) = this.copy(nameControl = mergeValue(nameControl, newValue));
  def updateNameAlternate(newValue: String) = this.copy(nameAlternate = mergeValue(nameAlternate, newValue));
  def updateNameFigment(newValue: String) = this.copy(nameFigment = mergeValue(nameFigment, newValue));
  def updateNameAbbreviation(newValue: String) = this.copy(nameAbbreviation = mergeValue(nameAbbreviation, newValue));
  def updateNameEmoji(newValue: String) = this.copy(nameEmoji = mergeValue(nameEmoji, newValue));
  def updateNameCustom(nameOpt: Option[String]): CodeInfo = nameOpt match {
    case Some(newValue) => this.copy(nameCustom = mergeValue(nameCustom, newValue));
    case None => this;
  }
  def updateGeneralCategory(newValue: String) = this.copy(generalCategory = mergeValue(generalCategory, newValue));
  def updateBlock(newValue: String) = this.copy(block = mergeValue(block, newValue));
  def updateScript(newValue: String) = this.copy(script = mergeValue(script, newValue));
  def updateBidiClass(newValue: String) = this.copy(bidiClass = mergeValue(bidiClass, newValue));
  def updateBidiMirroring(newValue: String) = this.copy(bidiMirroring = mergeValue(bidiMirroring, newValue));
  def updateEmojiPresentation() = this.copy(emojiPresentation = mergeValue(emojiPresentation, true));
  def updateMeaning(newValue: String) = this.copy(meaning = mergeValue(meaning, newValue));
  def updateMandarinReading(newValue: String) = this.copy(mandarinReading = mergeValue(mandarinReading, newValue));
  def updateCantoneseReading(newValue: String) = this.copy(cantoneseReading = mergeValue(cantoneseReading, newValue));
  def updateKoreanReading(newValue: String) = this.copy(koreanReading = mergeValue(koreanReading, newValue));
  def updateOption(newValue: String) = this.copy(option = mergeValue(option, newValue));

  override def toString: String = {
    val extra = CodeInfoExtra(this);
    val strT1 = this.asJson.noSpaces;
    val strX1 = extra.asJson.noSpaces;
    val strT2 = strT1.substring(1, strT1.length - 1);
    val strX2 = strX1.substring(1, strX1.length - 1);
    if (strT2 == "" || strX2 == "") {
      "{" + strT2 + strX2 + "}";
    } else {
      "{" + strT2 + "," + strX2 + "}";
    }
  }

  private[this] def mergeValue(currValue: Option[String], newValue: String): Option[String] = {
    currValue match {
      case Some(v) if (v == newValue) => currValue;
      case Some(v) => throw new Exception("%s != %s".format(v, newValue));
      case None => Some(newValue);
    }
  }

  @scala.annotation.targetName("mergeValueSeq")
  private[this] def mergeValue(currValue: Option[Seq[String]], newValue: String): Option[Seq[String]] = {
    currValue match {
      case Some(seq) => Some(seq :+ newValue);
      case None => Some(Seq(newValue));
    }
  }

  @scala.annotation.targetName("mergeValueBoolean")
  private[this] def mergeValue(currValue: Option[Boolean], newValue: Boolean): Option[Boolean] = {
    currValue match {
      case Some(v) if (v == newValue) => currValue;
      case Some(v) => throw new Exception("%s != %s".format(v, newValue));
      case None => Some(newValue);
    }
  }

}

object CodeInfo {

  private def empty = CodeInfo(None, None, None, None, None, None, None, None, None, None, None,
                               None, None, None, None, None, None, None, None);

  def updated(infoMap: Map[String, CodeInfo], code: String)(updator: CodeInfo => CodeInfo): Map[String, CodeInfo] = {
    val newInfo = updator(infoMap.getOrElse(code, CodeInfo.empty));
    infoMap.updated(code, newInfo);
  }

  implicit val encoder: Encoder[CodeInfo] = deriveEncoder[CodeInfo].mapJson(_.dropNullValues);

}

case class CodeInfoExtra(
  name: Option[String],
);

object CodeInfoExtra {
  def apply(info: CodeInfo) = new CodeInfoExtra(
    name = nameFromInfo(info),
  );
  implicit val encoder: Encoder[CodeInfoExtra] = deriveEncoder[CodeInfoExtra].mapJson(_.dropNullValues);

  def nameFromInfo(info: CodeInfo): Option[String] = {
    (info.nameDefault, info.nameCorrection, info.nameControl, info.nameEmoji, info.nameCustom) match {
      case (_, _, _, _, Some(name)) => Some(name);
      case (_, _, _, Some(name), _) => Some(name);
      case (_, Some(name), _, _, _) => Some(name);
      case (Some(name), _, _, _, _) => Some(name);
      case (_, _, Some(seq), _, _) => Some(seq.head);
      case _ => None;
    }
  }
}

def output(infoMap: Map[String, CodeInfo], path: String): Unit = {
  val codeList = infoMap.keys.toIndexedSeq.sortBy(code => {
    code.split(" ").toSeq.map { c =>
      if (c.length == 4) {
        "00" + c;
      } else if (c.length == 5) {
        "0" + c;
      } else if (c.length == 6) {
        c;
      } else {
        throw new Exception(code);
      }
    }.mkString("");
  });

  println(path);
  val fh = new PrintWriter(path);
  fh.println("{");

  val size = codeList.size;
  (0 until size).foreach { idx =>
    val code = codeList(idx);
    val suffix = if (idx == size - 1) {
      "";
    } else {
      ",";
    }
    fh.println("\"" + code + "\":" + infoMap(code).toString + suffix);
  }

  fh.println("}");
  fh.close();
}

