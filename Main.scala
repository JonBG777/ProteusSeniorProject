
import scala.util.matching.Regex
import scala.util.Either
sealed trait Keyword

object FPPKeywords {
  case object ENUM extends Keyword
  case object STRUCT extends Keyword
  case object ACTIVE extends Keyword
  case object ACTIVITY extends Keyword
  case object ALWAYS extends Keyword
  case object ASSERT extends Keyword
  case object ASYNC extends Keyword
  case object BASE extends Keyword
  case object CHANGE extends Keyword
  case object COMMAND extends Keyword
  case object COMPONENT extends Keyword
  case object CONNECTIONS extends Keyword
  case object CONTAINER extends Keyword
  case object CPU extends Keyword
  case object DEFAULT extends Keyword
  case object DIAGNOSTIC extends Keyword
  case object DROP extends Keyword
  case object FATAL extends Keyword
  case object FORMAT extends Keyword
  case object GET extends Keyword
  case object GUARDED extends Keyword
  case object HEALTH extends Keyword
  case object HIGH extends Keyword
  //case object ID extends Keyword
  //case object COMMAND extends Keyword
  //case object AT extends Keyword
  case object F32 extends Keyword
  //case object F64 extends Keyword
  //case object I16 extends Keyword
  //case object I32 extends Keyword
  //case object I64 extends Keyword
  //case object I8 extends Keyword
  //case object U16 extends Keyword
  //case object U32 extends Keyword
  //case object U64 extends Keyword
  //case object U8 extends Keyword


  val values: List[Keyword] = List(ENUM, STRUCT, ACTIVE, ACTIVITY, ALWAYS, ASSERT, ASYNC, BASE, CHANGE,COMMAND, COMPONENT, CONNECTIONS,
    CONTAINER, CPU, DEFAULT, DIAGNOSTIC, DROP, FATAL,FORMAT, GET, GUARDED, HEALTH, HIGH )
}


object ProteusKeywords {
  case object ACTOR extends Keyword
  case object STATEMACHINE extends Keyword
  case object STATENAME extends Keyword
  case object STATE extends Keyword
  case object EVENTMATCH extends Keyword
  case object ENTRY extends Keyword
  case object EXIT extends Keyword
  case object MONITOR extends Keyword
  case object GO extends Keyword
  case object GOIF extends Keyword


  val values: List[Keyword] = List(ACTOR, STATEMACHINE, STATENAME, STATE, EVENTMATCH, ENTRY, EXIT, MONITOR, GO, GOIF)
}

object UnknownKeywords {
  case object IF extends Keyword
  case object ELSE extends Keyword
  case object BOOL extends Keyword
  case object ARRAY extends Keyword
  case object CONSTANT extends Keyword
  case object BLOCK extends Keyword
  case object ON extends Keyword
  case object EVENT extends Keyword
  case object FALSE extends Keyword
  case object TRUE extends Keyword


  val values: List[Keyword] = List(IF, ELSE, BOOL, ARRAY, CONSTANT, BLOCK, ON, EVENT, FALSE, TRUE)
}
sealed trait Symbol

object Symbols {
  case object PLUS extends Symbol
  case object MINUS extends Symbol
  case object ASTERISK extends Symbol
  case object SLASH extends Symbol
  case object EQUALS extends Symbol
  case object LESS extends Symbol
  case object GREATER extends Symbol
  case object LBRACE extends Symbol
  case object RBRACE extends Symbol
  case object LPAREN extends Symbol
  case object RPAREN extends Symbol
  case object EMPTYBRACE extends Symbol

  val values: List[Symbol] = List(PLUS, MINUS, ASTERISK, SLASH, EQUALS, LESS, GREATER, LBRACE, RBRACE, LPAREN, RPAREN,EMPTYBRACE)
}

object Tokenizer {

  def tokenize(inputText: String): List[(Either[Keyword, Symbol], String)] =
    val tokens = inputText.split("\\s+").toList

    tokens.flatMap { token =>
      val fppMatch = FPPKeywords.values.find(_.toString.equalsIgnoreCase(token))
      val proteusMatch = ProteusKeywords.values.find(_.toString.equalsIgnoreCase(token))
      val unknownMatch = UnknownKeywords.values.find(_.toString.equalsIgnoreCase(token))
      val symbolMatch = token match {
        case "+" => Some(Right(Symbols.PLUS))
        case "-" => Some(Right(Symbols.MINUS))
        case "*" => Some(Right(Symbols.ASTERISK))
        case "/" => Some(Right(Symbols.SLASH))
        case "=" => Some(Right(Symbols.EQUALS))
        case "<" => Some(Right(Symbols.LESS))
        case ">" => Some(Right(Symbols.GREATER))
        case "{" => Some(Right(Symbols.LBRACE))
        case "}" => Some(Right(Symbols.RBRACE))
        case "(" => Some(Right(Symbols.LPAREN))
        case ")" => Some(Right(Symbols.RPAREN))
        case "{}" => Some(Right(Symbols.EMPTYBRACE))
        case _ => None
      }


        (fppMatch, proteusMatch, unknownMatch, symbolMatch) match {
          case (Some(keyword), _, _, _) => Some((Left(keyword), "FPP"))
          case (_, Some(keyword), _, _) => Some((Left(keyword), "Proteus"))
          case (_, _, Some(keyword), _) => Some((Left(keyword), "Unknown"))
          case (_, _, _, Some(symbol)) => Some((symbol, "Symbol"))
          case _ => None

      }



    }

  @main def runTokenizer: Unit =
    val inputText = "if actor {} enum struct statemachine else { - +"
    val result = Tokenizer.tokenize(inputText)
    result.foreach {
      case (keyword, "Proteus") =>
      println(s"Token: $keyword Language: Proteus")
      case (keyword, "FPP")  =>
        println(s"Token: $keyword Language: FPP")
      case (keyword, "Unknown") =>
        println(s"Token: $keyword Language: Unknown")
      case (symbol, "Symbol" ) =>
        println(s"Token: $symbol Language: Symbol")
      case (Left(identifier), "Identifier") =>
        println(s"Token: $identifier Language: Identifier")
      case _ => throw new Exception("Unmatched token category found.")

    }
}

/*
def Tokinize(List[Touples], Accum[Tokens): List[Tokens] = {}
  check List of Touples is empty
  if yes, return Accum
  else
  Pop First value in List of touples
  pattern match that with the tokens
  Create new token based on patern match
  Append new token to accum
  Call Tokinize on Tail of List of Touples and pass new accum

// Parser takes the above created list of tokens
actor A {\n statemachine {\n state S1
val listOfTokens = [Actor, Identifier("A"), LeftCurly, Statemachine, ...]
def parser (listOfTokens, FPPAccum, ProteusAccum, Context, Nested): List[List, List] =>
if listOfTokens is empty
return (FPPAccum, ProteusAccum)
else
pop the first token
match the context to be either Unknown, FPP, or Proteus
Case Unknown => match the tokens languege to either FPP or Proteus

 */






