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
  scriptExtension: Option[Seq[String]],

  upperCase: Option[String],
  lowerCase: Option[String],
  titleCase: Option[String],
  fullCaseFolding: Option[String],
  simpleCaseFolding: Option[String],
  turkicCaseFolding: Option[String],
  caseOf: Option[Seq[String]],

  canonicalCombiningClass: Option[Int],
  decompositionType: Option[String],
  decompositionMapping: Option[String],
  decompositionMappingNFD: Option[String],
  decompositionMappingNFKD: Option[String],
  canonicallyCompositionMapping: Option[String],
  otherCompositionMapping: Option[Seq[String]],

  // https://www.unicode.org/reports/tr44/tr44-30.html#Bidi_Class_Values
  bidiClass: Option[String],

  bidiMirroring: Option[String],

  cjkSemanticVariant: Option[Seq[String]],
  cjkSimplifiedVariant: Option[Seq[String]],
  cjkSpecializedSemanticVariant: Option[Seq[String]],
  cjkSpoofingVariant: Option[Seq[String]],
  cjkTraditionalVariant: Option[Seq[String]],
  cjkZVariant: Option[Seq[String]],

  meaning: Option[String],
  mandarinReading: Option[Seq[String]],
  cantoneseReading: Option[String],
  koreanReading: Option[Seq[String]],
  japaneseKunReading: Option[Seq[String]],
  japaneseOnReading: Option[Seq[String]],

  koreanHanja: Option[Seq[String]],

  unihanFlag: Option[Boolean],

  // 絵文字用のフォントで表示すべきかどうか
  emojiFont: Option[Boolean],
  emojiPresentation: Option[Boolean],
  emojiModifierBase: Option[Boolean],
  emojiGroup: Option[String],
  emojiSubgroup: Option[String],

  html: Option[String],
  option: Option[Seq[String]],

) {

  def updateName(newValue: String) = this.copy(name = mergeValue(name, newValue));
  def updateNameDefault(newValue: String) = this.copy(nameDefault = mergeValue(nameDefault, newValue));
  def updateNameCorrection(newValue: String) = this.copy(nameCorrection = mergeValue(nameCorrection, newValue));
  def updateNameControl(newValue: String) = this.copy(nameControl = mergeValue(nameControl, newValue));
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
  def updateScriptExtension(newValue: String) = this.copy(scriptExtension = mergeValue(scriptExtension, newValue));

  def updateUpperCase(newValue: String) = this.copy(upperCase = mergeValue(upperCase, newValue));
  def updateLowerCase(newValue: String) = this.copy(lowerCase = mergeValue(lowerCase, newValue));
  def updateTitleCase(newValue: String) = this.copy(titleCase = mergeValue(titleCase, newValue));
  def updateFullCaseFolding(newValue: String) = this.copy(fullCaseFolding = mergeValue(fullCaseFolding, newValue));
  def updateSimpleCaseFolding(newValue: String) = this.copy(simpleCaseFolding = mergeValue(simpleCaseFolding, newValue));
  def updateTurkicCaseFolding(newValue: String) = this.copy(turkicCaseFolding = mergeValue(turkicCaseFolding, newValue));
  def updateCaseOf(newValue: String) = this.copy(caseOf = mergeValue(caseOf, newValue));

  def updateCanonicalCombiningClass(newValue: Int) = this.copy(canonicalCombiningClass = mergeValue(canonicalCombiningClass, newValue));
  def updateDecompositionType(newValue: String) = this.copy(decompositionType = mergeValue(decompositionType, newValue));
  def updateDecompositionMapping(newValue: String) = this.copy(decompositionMapping = mergeValue(decompositionMapping, newValue));
  def updateDecompositionMappingNFD(newValue: String) = this.copy(decompositionMappingNFD = mergeValue(decompositionMappingNFD, newValue));
  def updateDecompositionMappingNFKD(newValue: String) = this.copy(decompositionMappingNFKD = mergeValue(decompositionMappingNFKD, newValue));
  def updateCanonicallyCompositionMapping(newValue: String) = this.copy(canonicallyCompositionMapping = mergeValue(canonicallyCompositionMapping, newValue));
  def updateOtherCompositionMapping(newValue: String) = this.copy(otherCompositionMapping = mergeValue(otherCompositionMapping, newValue));

  def updateBidiClass(newValue: String) = this.copy(bidiClass = mergeValue(bidiClass, newValue));
  def updateBidiMirroring(newValue: String) = this.copy(bidiMirroring = mergeValue(bidiMirroring, newValue));

  def updateCjkSemanticVariant(newValue: String) = this.copy(cjkSemanticVariant = mergeValue(cjkSemanticVariant, newValue));
  def updateCjkSimplifiedVariant(newValue: String) = this.copy(cjkSimplifiedVariant = mergeValue(cjkSimplifiedVariant, newValue));
  def updateCjkSpecializedSemanticVariant(newValue: String) = this.copy(cjkSpecializedSemanticVariant = mergeValue(cjkSpecializedSemanticVariant, newValue));
  def updateCjkSpoofingVariant(newValue: String) = this.copy(cjkSpoofingVariant = mergeValue(cjkSpoofingVariant, newValue));
  def updateCjkTraditionalVariant(newValue: String) = this.copy(cjkTraditionalVariant = mergeValue(cjkTraditionalVariant, newValue));
  def updateCjkZVariant(newValue: String) = this.copy(cjkZVariant = mergeValue(cjkZVariant, newValue));

  def updateMeaning(newValue: String) = this.copy(meaning = mergeValue(meaning, newValue));
  def updateMandarinReading(newValue: String) = this.copy(mandarinReading = mergeValue(mandarinReading, newValue));
  def updateCantoneseReading(newValue: String) = this.copy(cantoneseReading = mergeValue(cantoneseReading, newValue));
  def updateKoreanReading(newValue: String) = this.copy(koreanReading = mergeValue(koreanReading, newValue));
  def updateJapaneseKunReading(newValue: String) = this.copy(japaneseKunReading = mergeValue(japaneseKunReading, newValue));
  def updateJapaneseOnReading(newValue: String) = this.copy(japaneseOnReading = mergeValue(japaneseOnReading, newValue));
  def updateKoreanHanja(newValue: String) = this.copy(koreanHanja = mergeValue(koreanHanja, newValue));
  def updateUnihanFlag(newValue: Boolean) = this.copy(unihanFlag = mergeValue(unihanFlag, newValue));

  def updateEmojiFont(newValue: Boolean) = this.copy(emojiFont = mergeValue(emojiFont, newValue));
  def updateEmojiPresentation(newValue: Boolean) = this.copy(emojiPresentation = mergeValue(emojiPresentation, newValue));
  def updateEmojiModifierBase(newValue: Boolean) = this.copy(emojiModifierBase = mergeValue(emojiModifierBase, newValue));
  def updateEmojiGroup(newValue: String) = this.copy(emojiGroup = mergeValue(emojiGroup, newValue));
  def updateEmojiSubgroup(newValue: String) = this.copy(emojiSubgroup = mergeValue(emojiSubgroup, newValue));

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
      case Some(seq) if (seq.contains(newValue)) => Some(seq);
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

  @scala.annotation.targetName("mergeValueInt")
  private[this] def mergeValue(currValue: Option[Int], newValue: Int): Option[Int] = {
    currValue match {
      case Some(v) if (v == newValue) => currValue;
      case Some(v) => throw new Exception("%s != %s".format(v, newValue));
      case None => Some(newValue);
    }
  }

  def updateForCanonicallyComposition(other: CodeInfo): CodeInfo = {
    var result: CodeInfo = this;
    other.nameCorrection.foreach { newValue =>
      result = result.updateNameCorrection(newValue);
    }
    other.nameControl.getOrElse(Nil).foreach { newValue =>
      result = result.updateNameControl(newValue);
    }
    other.nameAlternate.foreach { newValue =>
      result = result.updateNameAlternate(newValue);
    }
    other.nameFigment.foreach { newValue =>
      result = result.updateNameFigment(newValue);
    }
    other.nameAbbreviation.getOrElse(Nil).foreach { newValue =>
      result = result.updateNameAbbreviation(newValue);
    }
    other.nameEmoji.foreach { newValue =>
      result = result.updateNameEmoji(newValue);
    }
    other.nameCustom.foreach { newValue =>
      result = result.updateNameCustom(Some(newValue));
    }
    other.generalCategory.foreach { newValue =>
      result = result.updateGeneralCategory(newValue);
    }
    other.script.foreach { newValue =>
      result = result.updateScript(newValue);
    }
    other.scriptExtension.getOrElse(Nil).foreach { newValue =>
      result = result.updateScriptExtension(newValue);
    }
    other.upperCase.foreach { newValue =>
      result = result.updateUpperCase(newValue);
    }
    other.lowerCase.foreach { newValue =>
      result = result.updateLowerCase(newValue);
    }
    other.titleCase.foreach { newValue =>
      result = result.updateTitleCase(newValue);
    }
    other.fullCaseFolding.foreach { newValue =>
      result = result.updateFullCaseFolding(newValue);
    }
    other.simpleCaseFolding.foreach { newValue =>
      result = result.updateSimpleCaseFolding(newValue);
    }
    other.turkicCaseFolding.foreach { newValue =>
      result = result.updateTurkicCaseFolding(newValue);
    }
    other.bidiClass.foreach { newValue =>
      result = result.updateBidiClass(newValue);
    }
    other.bidiMirroring.foreach { newValue =>
      result = result.updateBidiMirroring(newValue);
    }
    other.emojiFont.foreach { newValue =>
      result = result.updateEmojiFont(newValue);
    }
    other.emojiPresentation.foreach { newValue =>
      result = result.updateEmojiPresentation(newValue);
    }
    other.emojiModifierBase.foreach { newValue =>
      result = result.updateEmojiModifierBase(newValue);
    }
    other.emojiGroup.foreach { newValue =>
      result = result.updateEmojiGroup(newValue);
    }
    other.emojiSubgroup.foreach { newValue =>
      result = result.updateEmojiSubgroup(newValue);
    }
    other.html.foreach { newValue =>
      result = result.updateHtml(newValue);
    }
    result;
  }

}

object CodeInfo {

  def empty = CodeInfo(None, None, None, None, None, None, None, None, None, None, None,
                       None, None, None, None, None, None, None, None, None, None, None,
                       None, None, None, None, None, None, None, None, None, None, None,
                       None, None, None, None, None, None, None, None, None, None, None,
                       None, None, None, None, None, None);

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

