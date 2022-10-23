import org.opalj.br.Method
import scala.collection.mutable.ListBuffer

class ReflectionInvoke extends ReflectionUse {
  override var className: String = _
  override var method: Method = _
  override var nameReflectionFunction: Option[String] = _
  override var byteCodeInfo: Option[ByteCodeInfo] = _
  override var fieldAndClass: Option[FieldAndClass] = _
  override var methodAndClass: Option[MethodAndClass] = _
  override var classConstructor: Option[ClassConstructor] = _
  override var isValid: Boolean = true
  var modifiedObject: String = _
  var valueObjects: ListBuffer[String] = new ListBuffer[String]()
  var objectInfo: ByteCodeInfo = _
  var valueInfo: ListBuffer[ByteCodeInfo] = new ListBuffer[ByteCodeInfo]()
}
