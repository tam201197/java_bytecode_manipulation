import scala.collection.mutable.ListBuffer
import org.opalj.br._

class ReflectionSet extends ReflectionUse {
  override var className: String = ""
  override var method: Method = _
  override var nameReflectionFunction: Option[String] = None
  override var byteCodeInfo: Option[ByteCodeInfo] = None
  override var fieldAndClass: Option[FieldAndClass] = None
  override var methodAndClass: Option[MethodAndClass] = None
  override var classConstructor: Option[ClassConstructor] = None
  override var isValid: Boolean = true
  var modifiedObject: ListBuffer[String] = new ListBuffer[String]()
  var valueObject: ListBuffer[String] = new ListBuffer[String]()
  var objectInfo: ListBuffer[ByteCodeInfo] = new ListBuffer[ByteCodeInfo]()
  var valueInfo: ListBuffer[ByteCodeInfo] = new ListBuffer[ByteCodeInfo]()
}
