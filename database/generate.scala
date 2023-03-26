import java.io.PrintWriter;

import scala.collection.immutable.SeqMap;
import scala.io.Source;
import scala.util.Using;

@main def main: Unit = {
  val codePointInfoMap: Map[String, CodeInfo] = {
    val singleCodePointInfoMap: Map[String, CodeInfo] = (
      fetchUnicodeData("var/UnicodeData.txt") ++
      fetchBidiMirroring("var/BidiMirroring.txt") ++
      fetchNameAliases("var/NameAliases.txt") ++
      fetchScripts("var/Scripts.txt") ++
      fetchScriptExtensions("var/ScriptExtensions.txt") ++
      fetchPropList("var/PropList.txt") ++
      fetchEmojiData("var/emoji-data.txt") ++
      fetchUnihanReadings("var/Unihan_Readings.txt") ++
      Nil
    ).foldLeft(Map.empty) { (infoMap, entry) =>
      val (code, updator) = entry;
      CodeInfo.updated(infoMap, code)(updator);
    }

    val singleCodePointWithBlockInfoMap: Map[String, CodeInfo] = (
      fetchBlocks(singleCodePointInfoMap, "var/Blocks.txt")
    ).foldLeft(singleCodePointInfoMap) { (infoMap, entry) =>
      val (code, updator) = entry;
      CodeInfo.updated(infoMap, code)(updator);
    }

    val sequenceCodePointsInfoMap: Map[String, CodeInfo] = (
      fetchCaseFolding("var/CaseFolding.txt") ++
      fetchSpecialCasing(singleCodePointWithBlockInfoMap, "var/SpecialCasing.txt") ++
      fetchEmojiSequences(singleCodePointWithBlockInfoMap, "var/emoji-sequences.txt") ++
      fetchEmojiVariationSequences(singleCodePointWithBlockInfoMap, "var/emoji-variation-sequences.txt") ++
      fetchEmojiZwjSequences(singleCodePointWithBlockInfoMap, "var/emoji-zwj-sequences.txt") ++
      fetchEmojiTest(singleCodePointWithBlockInfoMap, "var/emoji-test.txt") ++
      Nil
    ).foldLeft(singleCodePointWithBlockInfoMap) { (infoMap, entry) =>
      val (code, updator) = entry;
      CodeInfo.updated(infoMap, code)(updator);
    }

    val sequenceCodePointsWithDecompositionMappingInfoMap: Map[String, CodeInfo] = {
      val infoMap1 = sequenceCodePointsInfoMap;
      val infoMap2: Map[String, CodeInfo] = (
        selectDecompositionMapping(infoMap1)
      ).foldLeft(infoMap1) { (infoMap, entry) =>
        val (code, updator) = entry;
        CodeInfo.updated(infoMap, code)(updator);
      }
      val infoMap3: Map[String, CodeInfo] = (
        selectCompositionMapping(infoMap2)
      ).foldLeft(infoMap2) { (infoMap, entry) =>
        val (code, updator) = entry;
        CodeInfo.updated(infoMap, code)(updator);
      }
      infoMap3;
    }

    val codePointInfoMap: Map[String, CodeInfo] = (
      selectMathematicalSymbols(sequenceCodePointsWithDecompositionMappingInfoMap) ++
      selectArrowSymbols(sequenceCodePointsWithDecompositionMappingInfoMap) ++
      selectEmojiCharacters(sequenceCodePointsWithDecompositionMappingInfoMap) ++
      selectCharacterInfoName(sequenceCodePointsWithDecompositionMappingInfoMap) ++
      Nil
    ).foldLeft(sequenceCodePointsWithDecompositionMappingInfoMap) { (infoMap, entry) =>
      val (code, updator) = entry;
      CodeInfo.updated(infoMap, code)(updator);
    }

    codePointInfoMap;
  }

  val infoMap: Map[String, CodeInfo] = codePointInfoMap;

  output(infoMap, "data/all.json");
}

def fetchUnicodeData(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 15).filter { case (line, cols) =>
    val name = cols(1);
    !(name.startsWith("<") && name != "<control>");
  }.flatMap { case (line, cols) =>
    var result: Seq[(String, CodeInfo => CodeInfo)] = Nil;
    val code = cols(0);

    {
      val name = cols(1);
      if (name != "<control>") {
        result = result :+ (code, codeInfo => codeInfo.updateNameDefault(name));
      }
    }

    {
      val generalCategory = cols(2);
      result = result :+ (code, codeInfo => codeInfo.updateGeneralCategory(generalCategory));
    }
    {
      val bidiClass = cols(4);
      result = result :+ (code, codeInfo => codeInfo.updateBidiClass(bidiClass));
    }
    {
      val upperCase = cols(12);
      val lowerCase = cols(13);
      val titleCase = cols(14);
      if (upperCase != "") {
        result = result :+ (code, codeInfo => codeInfo.updateUpperCase(upperCase));
        result = result :+ (upperCase, codeInfo => codeInfo.updateCaseOf(code));
      }
      if (lowerCase != "") {
        result = result :+ (code, codeInfo => codeInfo.updateLowerCase(lowerCase));
        result = result :+ (lowerCase, codeInfo => codeInfo.updateCaseOf(code));
      }
      if (titleCase != "") {
        result = result :+ (code, codeInfo => codeInfo.updateTitleCase(titleCase));
        result = result :+ (titleCase, codeInfo => codeInfo.updateCaseOf(code));
      }
    }

    if (cols(3) != "") {
      val canonicalCombiningClass = Integer.parseInt(cols(3));
      if (canonicalCombiningClass != 0) {
        result = result :+ (code, codeInfo => codeInfo.updateCanonicalCombiningClass(canonicalCombiningClass));
      }
    }

    if (cols(5) != "") {
      val cs = cols(5).split(" ").toSeq;
      val cs0 = cs(0);
      val (decompositionType, decompositionMapping) = if (cs0.startsWith("<") && cs0.endsWith(">")) {
        val decompositionType = cs0.substring(1, cs0.length - 1);
        val decompositionMapping = cs.slice(1, cs.length);
        (decompositionType, decompositionMapping);
      } else {
        ("canonical", cs);
      }
      val decompositionMappingStr = decompositionMapping.mkString(" ");
      result = result :+ (code, codeInfo => codeInfo.
        updateDecompositionType(decompositionType).updateDecompositionMapping(decompositionMappingStr));
    }

    result;
  }
}

def fetchCaseFolding(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 4).flatMap { case (line, cols) =>
    val capital = cols(0);
    val small = cols(2);
    if (cols(1) == "C") {
      Seq[(String, CodeInfo => CodeInfo)](
        (capital, codeInfo => codeInfo.updateFullCaseFolding(small)),
        (capital, codeInfo => codeInfo.updateSimpleCaseFolding(small)),
        (small, codeInfo => codeInfo.updateCaseOf(capital)),
      );
    } else if (cols(1) == "F") {
      Seq[(String, CodeInfo => CodeInfo)](
        (capital, codeInfo => codeInfo.updateFullCaseFolding(small)),
        (small, codeInfo => codeInfo.updateCaseOf(capital)),
      );
    } else if (cols(1) == "S") {
      Seq[(String, CodeInfo => CodeInfo)](
        (capital, codeInfo => codeInfo.updateSimpleCaseFolding(small)),
        (small, codeInfo => codeInfo.updateCaseOf(capital)),
      );
    } else if (cols(1) == "T") {
      Seq[(String, CodeInfo => CodeInfo)](
        (capital, codeInfo => codeInfo.updateTurkicCaseFolding(small)),
        (small, codeInfo => codeInfo.updateCaseOf(capital)),
      );
    } else {
      Nil;
    }
  }
}

def fetchSpecialCasing(codePointInfoMap: Map[String, CodeInfo], path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 5).flatMap { case (line, cols) =>
    val code = cols(0);
    val lower = cols(1);
    val title = cols(2);
    val upper = cols(3);
    val conditions = cols(4);
    var result: Seq[(String, CodeInfo => CodeInfo)] = Nil;
    if (conditions == "") {
      if (lower != "" && lower != code) {
        if (codePointInfoMap(code).lowerCase == None) {
          result = result :+ (code, codeInfo => codeInfo.updateLowerCase(lower));
          result = result :+ (lower, codeInfo => codeInfo.updateCaseOf(code));
        }
      }
      if (title != "" && title != code) {
        if (codePointInfoMap(code).titleCase == None) {
          result = result :+ (code, codeInfo => codeInfo.updateTitleCase(title));
          result = result :+ (title, codeInfo => codeInfo.updateCaseOf(code));
        }
      }
      if (upper != "" && upper != code) {
        if (codePointInfoMap(code).upperCase == None) {
          result = result :+ (code, codeInfo => codeInfo.updateUpperCase(upper));
          result = result :+ (upper, codeInfo => codeInfo.updateCaseOf(code));
        }
      }
    }
    result;
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

def fetchScriptExtensions(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 2).flatMap { case (line, cols) =>
    val codePoints = cols(0).split("\\.\\.");
    val (rangeFirst, rangeList) = if (codePoints.length >= 2) {
      (Integer.parseInt(codePoints(0), 16), Integer.parseInt(codePoints(1), 16));
    } else {
      val c = Integer.parseInt(codePoints(0), 16);
      (c, c);
    }
    val scriptNames = cols(1).split(" ").toSeq;
    (rangeFirst to rangeList).flatMap { c =>
      scriptNames.map { scriptName =>
        (codePointToCode(c), (codeInfo: CodeInfo) => codeInfo.updateScriptExtension(scriptName));
      }
    }
  }
}

def fetchPropList(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile(path, 2).flatMap { case (line, cols) =>
    val codePoints = cols(0).split("\\.\\.");
    val (rangeFirst, rangeList) = if (codePoints.length >= 2) {
      (Integer.parseInt(codePoints(0), 16), Integer.parseInt(codePoints(1), 16));
    } else {
      val c = Integer.parseInt(codePoints(0), 16);
      (c, c);
    }
    val optionName = cols(1);
    (rangeFirst to rangeList).map { c =>
      (codePointToCode(c), (codeInfo: CodeInfo) => codeInfo.updateOption(optionName));
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

def selectCharacterInfoName(codePointInfoMap: Map[String, CodeInfo]): Seq[(String, CodeInfo => CodeInfo)] = {
  codePointInfoMap.toSeq.flatMap { case (code, info) =>
    selectName(info).map { name =>
      (code, (codeInfo: CodeInfo) => codeInfo.updateName(name));
    }
  }
}

def selectDecompositionMapping(codePointInfoMap: Map[String, CodeInfo]): Seq[(String, CodeInfo => CodeInfo)] = {
  def fetchDecompositionMapping(code: String, isCanonical: Boolean): String = {
    val info = codePointInfoMap(code);
    info.decompositionMapping match {
      case None => code;
      case Some(decompositionMapping) =>
        if (isCanonical && info.decompositionType != Some("canonical")) {
          code;
        } else {
          val mapping = decompositionMapping.split(" ");
          mapping.map(fetchDecompositionMapping(_, isCanonical)).mkString(" ");
        }
    }
  }
  def reorder(code: String): String = {
    val c1: Seq[(Int, String, Int)] = code.split(" ").toSeq.zipWithIndex.map { case (c, idx) =>
      (idx, c, codePointInfoMap(c).canonicalCombiningClass.getOrElse(0));
    }
    val baseIdxList: Seq[Int] = c1.filter(t => t._1 == 0 || t._3 == 0).map(_._1);
    (0 until baseIdxList.size).map { idx =>
      val start = baseIdxList(idx);
      val end = if (idx == baseIdxList.size - 1) c1.size else baseIdxList(idx + 1);
      c1.slice(start, end).sortBy(t => (t._3, t._1)).map(_._2).mkString(" ");
    }.mkString(" ");
  }
  codePointInfoMap.toSeq.flatMap { case (code, info) =>
    info.decompositionMapping match {
      case None => Nil;
      case Some(_) =>
        val nfd = reorder(fetchDecompositionMapping(code, true));
        val nfkd = reorder(fetchDecompositionMapping(code, false));
        var result = Seq[(String, CodeInfo => CodeInfo)]();
        if (nfd != code) {
          result = result :+ (code, (codeInfo: CodeInfo) => codeInfo.updateDecompositionMappingNFD(nfd));
        }
        if (nfkd != code) {
          result = result :+ (code, (codeInfo: CodeInfo) => codeInfo.updateDecompositionMappingNFKD(nfkd));
        }
        result;
    }
  }
}

def selectCompositionMapping(codePointInfoMap: Map[String, CodeInfo]): Seq[(String, CodeInfo => CodeInfo)] = {
  Nil; // TODO
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

def usingDataFile3(path: String, colCount: Int): Seq[(String, Option[Seq[String]])] = {
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
        if (line == "") {
          None;
        } else {
          Some((line, None));
        }
      } else {
        val cols = line2.split(";", colCount).toIndexedSeq.map(_.trim);
        if (cols.size != colCount) {
          throw new Exception(line);
        }
        Some((line, Some(cols)));
      }
    }.toSeq;
  }.get;
}

def selectName(info: CodeInfo): Option[String] = {
  (info.nameDefault, info.nameCorrection, info.nameControl, info.nameEmoji, info.nameCustom) match {
    case (_, _, _, _, Some(name)) => Some(name);
    case (_, _, _, Some(name), _) => Some(name);
    case (_, Some(name), _, _, _) => Some(name);
    case (Some(name), _, _, _, _) => Some(name);
    case (_, _, Some(seq), _, _) => Some(seq.head);
    case _ => None;
  }
}

def buildCombiningName(codePoints: Seq[Int], codePointInfoMap: Map[String, CodeInfo]): Option[String] = {
  val nameOpts: Seq[Option[String]] = codePoints.map { codePoint =>
    codePointInfoMap.get(codePointToCode(codePoint)).flatMap(info => selectName(info));
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

