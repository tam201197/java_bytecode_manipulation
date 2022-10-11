import scala.collection.mutable.{ListBuffer, Map}
import org.opalj.br._

trait ReflectionUse {
  var className: String
  var method: Method
  var nameReflectionFunction : Option[String]
  var byteCodeInfo: Option[ByteCodeInfo]
  var fieldAndClass: Option[FieldAndClass]
  var methodAndClass: Option[MethodAndClass]
  var classConstructor: Option[ClassConstructor]
  var isValid: Boolean
}
