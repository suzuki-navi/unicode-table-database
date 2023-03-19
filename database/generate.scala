import java.io.PrintWriter;

import scala.collection.immutable.SeqMap;
import scala.io.Source;
import scala.util.Using;

import io.circe.Encoder;
import io.circe.syntax._;
import io.circe.generic.auto._;
import io.circe.generic.semiauto.deriveEncoder;

@main def main: Unit = {
  val infoMap: Map[String, CodeInfo] = (
    fetchUnicodeData("var/UnicodeData.txt") ++
    fetchNameAliases("var/NameAliases.txt") ++
    Nil
  ).foldLeft(Map.empty) { (infoMap, entry) =>
    val (code, updator) = entry;
    CodeInfo.updated(infoMap, code)(updator);
  }
  output(infoMap, "data/all.json");
}

def fetchUnicodeData(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  Using(Source.fromFile(path)) { source =>
    val colCount = 15;
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
        val cols = line2.split(";", colCount).map(_.trim);
        if (cols.size != colCount) {
          throw new Exception(line);
        }
        Some(cols);
      }
    }.filter { cols =>
      val name = cols(1);
      !(name.startsWith("<") && name != "<control>");
    }.flatMap { cols =>
      val code = cols(0);
      val name = if (cols(1) == "<control>") None else Some(cols(1));
      val generalCategory = cols(2);
      val bidiClass = cols(4);
      Seq[(String, CodeInfo => CodeInfo)](
        (code, codeInfo => {
          codeInfo.updateGeneralCategory(generalCategory).
            updateBidiClass(bidiClass);
        }),
      ) ++ name.map(name => (code, (codeInfo: CodeInfo) => {
        codeInfo.updateNameDefault(name);
      }));
    }.toSeq;
  }.get;
}

def fetchNameAliases(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  Using(Source.fromFile(path)) { source =>
    val colCount = 3;
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
        val cols = line2.split(";", colCount).map(_.trim);
        if (cols.size != colCount) {
          throw new Exception(line);
        }
        Some(cols);
      }
    }.filter { cols =>
      val name = cols(1);
      !(name.startsWith("<") && name != "<control>");
    }.flatMap { cols =>
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
    }.toSeq;
  }.get;
}

case class CodeInfo(
  nameDefault: Option[String],

  // https://www.unicode.org/Public/15.0.0/ucd/NameAliases.txt
  nameCorrection: Option[String],
  nameControl: Option[Seq[String]],
  nameAlternate: Option[String],
  nameFigment: Option[String],
  nameAbbreviation: Option[Seq[String]],

  // https://www.unicode.org/reports/tr44/tr44-30.html#General_Category_Values
  generalCategory: Option[String],

  block: Option[String],

  script: Option[String],

  // https://www.unicode.org/reports/tr44/tr44-30.html#Bidi_Class_Values
  bidiClass: Option[String],

) {

  def updateNameDefault(newValue: String) = this.copy(nameDefault = mergeValue(nameDefault, newValue));
  def updateNameCorrection(newValue: String) = this.copy(nameCorrection = mergeValue(nameCorrection, newValue));
  def appendNameControl(newValue: String) = this.copy(nameControl = mergeValue(nameControl, newValue));
  def updateNameAlternate(newValue: String) = this.copy(nameAlternate = mergeValue(nameAlternate, newValue));
  def updateNameFigment(newValue: String) = this.copy(nameFigment = mergeValue(nameFigment, newValue));
  def updateNameAbbreviation(newValue: String) = this.copy(nameAbbreviation = mergeValue(nameAbbreviation, newValue));
  def updateGeneralCategory(newValue: String) = this.copy(generalCategory = mergeValue(generalCategory, newValue));
  def updateBidiClass(newValue: String) = this.copy(bidiClass = mergeValue(bidiClass, newValue));

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

}

object CodeInfo {

  private def empty = CodeInfo(None, None, None, None, None, None, None, None, None, None);

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
    name = {
      (info.nameDefault, info.nameCorrection) match {
        case (_, Some(name)) => Some(name);
        case (v, _) => v;
      }
    },
  );
  implicit val encoder: Encoder[CodeInfoExtra] = deriveEncoder[CodeInfoExtra].mapJson(_.dropNullValues);
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

