
def fetchChiseData(path: String): Seq[(String, CodeInfo => CodeInfo)] = {
  usingDataFile4(path, 3).filter { case (line, cols) =>
    cols(0).startsWith("U+");
  }.map { case (line, cols) =>
    val code = cols(0).substring(2);
    val descriptionSequence = cols(2);
    (code, (codeInfo: CodeInfo) => codeInfo.updateChiseDescriptionSequence(descriptionSequence));
  }
}

