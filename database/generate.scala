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
      fetchEmojiSequences(singleCodePointWithBlockInfoMap, "var/emoji-sequences.txt") ++
      fetchEmojiVariationSequences(singleCodePointWithBlockInfoMap, "var/emoji-variation-sequences.txt") ++
      fetchEmojiZwjSequences(singleCodePointWithBlockInfoMap, "var/emoji-zwj-sequences.txt") ++
      Nil
    ).foldLeft(singleCodePointWithBlockInfoMap) { (infoMap, entry) =>
      val (code, updator) = entry;
      CodeInfo.updated(infoMap, code)(updator);
    }

    val codePointInfoMap: Map[String, CodeInfo] = (
      selectMathematicalSymbols(sequenceCodePointsInfoMap) ++
      selectArrowSymbols(sequenceCodePointsInfoMap) ++
      selectCharacterInfoName(sequenceCodePointsInfoMap) ++
      Nil
    ).foldLeft(sequenceCodePointsInfoMap) { (infoMap, entry) =>
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

