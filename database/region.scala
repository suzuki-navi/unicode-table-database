
def assignRegion(codePointInfoMap: Map[String, CodeInfo]): Seq[(String, CodeInfo => CodeInfo)] = {
  // temporary implementation
  codePointInfoMap.toSeq.flatMap { case (code, info) =>
    val region: Option[String] = info.script match {
      case None => None;
      case Some(script) => script match {
        case "Latin" => Some("europe");
        case "Greek" => Some("europe");
        case "Cyrillic" => Some("europe");
        case "Armenian" => Some("europe");
        case "Georgian" => Some("europe");
        case "Arabic" => Some("middleeast");
        case "Hebrew" => Some("middleeast");
        case "Syriac" => Some("middleeast");
        case "Samaritan" => Some("middleeast");
        case "Mandaic" => Some("middleeast");
        case "Han" => Some("eastasia");
        case "Hiragana" => Some("eastasia");
        case "Katakana" => Some("eastasia");
        case "Hangul" => Some("eastasia");
        case "Yi" => Some("eastasia");
        case "Nushu" => Some("eastasia");
        case "Miao" => Some("eastasia");
        case "Bopomofo" => Some("eastasia");
        case "Lisu" => Some("eastasia");
        case "Tibetan" => Some("centralasia");
        case "Mongolian" => Some("centralasia");
        case "Devanagari" => Some("southasia");
        case "Khmer" => Some("southasia");
        case "Tamil" => Some("southasia");
        case "Malayalam" => Some("southasia");
        case "Sinhala" => Some("southasia");
        case "Telugu" => Some("southasia");
        case "Newa" => Some("southasia");
        case "Bengali" => Some("southasia");
        case "Sharada" => Some("southasia");
        case "Gujarati" => Some("southasia");
        case "Kannada" => Some("southasia");
        case "Oriya" => Some("southasia");
        case "Warang_Citi" => Some("southasia");
        case "Saurashtra" => Some("southasia");
        case "Tirhuta" => Some("southasia");
        case "Gurmukhi" => Some("southasia");
        case "Meetei_Mayek" => Some("southasia");
        case "Masaram_Gondi" => Some("southasia");
        case "Lepcha" => Some("southasia");
        case "Chakma" => Some("southasia");
        case "Khudawadi" => Some("southasia");
        case "Takri" => Some("southasia");
        case "Gunjala_Gondi" => Some("southasia");
        case "Wancho" => Some("southasia");
        case "Thaana" => Some("southasia");
        case "Ol_Chiki" => Some("southasia");
        case "Mro" => Some("southasia");
        case "Nag_Mundari" => Some("southasia");
        case "Sora_Sompeng" => Some("southasia");
        case "Toto" => Some("southasia");
        case "Myanmar" => Some("southeastasia");
        case "Pahawh_Hmong" => Some("southeastasia");
        case "Tai_Tham" => Some("southeastasia");
        case "Balinese" => Some("southeastasia");
        case "Javanese" => Some("southeastasia");
        case "Tangsa" => Some("southeastasia");
        case "Thai" => Some("southeastasia");
        case "Cham" => Some("southeastasia");
        case "Lao" => Some("southeastasia");
        case "New_Tai_Lue" => Some("southeastasia");
        case "Sundanese" => Some("southeastasia");
        case "Tai_Viet" => Some("southeastasia");
        case "Nyiakeng_Puachue_Hmong" => Some("southeastasia");
        case "Pau_Cin_Hau" => Some("southeastasia");
        case "Batak" => Some("southeastasia");
        case "Hanifi_Rohingya" => Some("southeastasia");
        case "Kayah_Li" => Some("southeastasia");
        case "Rejang" => Some("southeastasia");
        case "Tai_Le" => Some("southeastasia");
        case "Buginese" => Some("southeastasia");
        case "Tagalog" => Some("southeastasia");
        case "Hanunoo" => Some("southeastasia");
        case "Buhid" => Some("southeastasia");
        case "Tagbanwa" => Some("southeastasia");
        case "Bamum" => Some("africa");
        case "Ethiopic" => Some("africa");
        case "Vai" => Some("africa");
        case "Mende_Kikakui" => Some("africa");
        case "Medefaidrin" => Some("africa");
        case "Adlam" => Some("africa");
        case "Nko" => Some("africa");
        case "Tifinagh" => Some("africa");
        case "Osmanya" => Some("africa");
        case "Canadian_Aboriginal" => Some("america");
        case "Cherokee" => Some("america");
        case "Deseret" => Some("america");
        case "Osage" => Some("america");
        case _ => None;
      }
    }
    region match {
      case None => None;
      case Some(region) => Some((code, (codeInfo: CodeInfo) => codeInfo.updateRegion(region)));
    }
  };
}

