package todowriter

/** Represents validation issues found in Scaladoc comments. */
enum Issue:
  case MissingParam(names: List[String])
  case UnknownParam(names: List[String])
  case MissingTparam(names: List[String])
  case UnknownTparam(names: List[String])
  case MissingReturn
  case UnnecessaryReturn

  def message: String = this match
    case MissingParam(names)  => s"Missing @param for: ${names.mkString(", ")}"
    case UnknownParam(names)  => s"@param refers to unknown params: ${names.mkString(", ")}"
    case MissingTparam(names) => s"Missing @tparam for: ${names.mkString(", ")}"
    case UnknownTparam(names) => s"@tparam refers to unknown type params: ${names.mkString(", ")}"
    case MissingReturn        => "Missing @return for non-Unit return type"
    case UnnecessaryReturn    => "@return present but return type is Unit"
