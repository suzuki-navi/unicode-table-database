import io.circe.Decoder;
import io.circe.Encoder;
import io.circe.parser.decode;
import io.circe.syntax._;
import io.circe.generic.semiauto.deriveDecoder;
import io.circe.generic.semiauto.deriveEncoder;

case class CodeInfo(
  name: Option[String],

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
  emojiModifierBase: Option[Boolean],

  emojiGroup: Option[String],
  emojiSubgroup: Option[String],

  meaning: Option[String],
  mandarinReading: Option[Seq[String]],
  cantoneseReading: Option[String],
  koreanReading: Option[Seq[String]],

  koreanHanja: Option[Seq[String]],

  html: Option[String],
  option: Option[Seq[String]],

) {

  def updateName(newValue: String) = this.copy(name = mergeValue(name, newValue));
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
  def updateEmojiModifierBase() = this.copy(emojiModifierBase = mergeValue(emojiModifierBase, true));
  def updateEmojiGroup(newValue: String) = this.copy(emojiGroup = mergeValue(emojiGroup, newValue));
  def updateEmojiSubgroup(newValue: String) = this.copy(emojiSubgroup = mergeValue(emojiSubgroup, newValue));
  def updateMeaning(newValue: String) = this.copy(meaning = mergeValue(meaning, newValue));
  def updateMandarinReading(newValue: String) = this.copy(mandarinReading = mergeValue(mandarinReading, newValue));
  def updateCantoneseReading(newValue: String) = this.copy(cantoneseReading = mergeValue(cantoneseReading, newValue));
  def updateKoreanReading(newValue: String) = this.copy(koreanReading = mergeValue(koreanReading, newValue));
  def updateKoreanHanja(newValue: String) = this.copy(koreanHanja = mergeValue(koreanHanja, newValue));
  def updateHtml(newValue: String) = this.copy(html = mergeValue(html, newValue));
  def updateOption(newValue: String) = this.copy(option = mergeValue(option, newValue));

  override def toString: String = {
    this.asJson.noSpaces;
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
                               None, None, None, None, None, None, None, None, None, None, None,
                               None, None, None);

  def updated(infoMap: Map[String, CodeInfo], code: String)(updator: CodeInfo => CodeInfo): Map[String, CodeInfo] = {
    val newInfo = updator(infoMap.getOrElse(code, CodeInfo.empty));
    infoMap.updated(code, newInfo);
  }

  implicit val encoder: Encoder[CodeInfo] = deriveEncoder[CodeInfo].mapJson(_.dropNullValues);
  implicit val decoder: Decoder[CodeInfo] = deriveDecoder[CodeInfo];

  def parse(jsonStr: String): Option[CodeInfo] = {
    decode[CodeInfo](jsonStr) match {
      case Left(e) => None;
      case Right(info) => Some(info);
    }
  }

}

