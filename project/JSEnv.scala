sealed abstract class JSEnv
object JSEnv {
  case object Chrome extends JSEnv
  case object Firefox extends JSEnv
  case object JSDOM extends JSEnv
}
