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
    Nil
  ).foldLeft(Map.empty) { (infoMap, entry) =>
    val (code, updator) = entry;
    CodeInfo.updated(infoMap, code)(updator);
  }
  output(infoMap, "data/all.json");
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

def fetchUnicodeData(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  Using(Source.fromFile(path)) { source =>
    source.getLines().map(_.split(";", 15)).
    filter { cols =>
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
        codeInfo.updateName(name);
      }));
    }.toSeq;
  }.get;
}

case class CodeInfo(
  name: Option[String],
  generalCategory: Option[String],
  block: Option[String],
  script: Option[String],
  bidiClass: Option[String],
) {

  def updateName(newValue: String): CodeInfo = {
    this.name match {
      case Some(v) if (v == newValue) => this;
      case Some(v) => throw new Exception("%s != %s".format(v, newValue));
      case None => this.copy(name = Some(newValue));
    }
  }

  def updateGeneralCategory(newValue: String): CodeInfo = {
    this.generalCategory match {
      case Some(v) if (v == newValue) => this;
      case Some(v) => throw new Exception("%s != %s".format(v, newValue));
      case None => this.copy(generalCategory = Some(newValue));
    }
  }

  def updateBidiClass(newValue: String): CodeInfo = {
    this.bidiClass match {
      case Some(v) if (v == newValue) => this;
      case Some(v) => throw new Exception("%s != %s".format(v, newValue));
      case None => this.copy(bidiClass = Some(newValue));
    }
  }

  override def toString: String = {
    this.asJson.noSpaces;
  }

}

object CodeInfo {

  private def empty = CodeInfo(None, None, None, None, None);

  def updated(infoMap: Map[String, CodeInfo], code: String)(updator: CodeInfo => CodeInfo): Map[String, CodeInfo] = {
    val newInfo = updator(infoMap.getOrElse(code, CodeInfo.empty));
    infoMap.updated(code, newInfo);
  }

  implicit val encoder: Encoder[CodeInfo] = deriveEncoder[CodeInfo].mapJson(_.dropNullValues);

}

