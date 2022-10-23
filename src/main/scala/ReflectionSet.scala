import org.opalj.br._

class ReflectionSet extends ReflectionUse {
  override var className: String = _
  override var method: Method = _
  override var nameReflectionFunction: Option[String] = None
  override var byteCodeInfo: Option[ByteCodeInfo] = None
  override var fieldAndClass: Option[FieldAndClass] = None
  override var methodAndClass: Option[MethodAndClass] = None
  override var classConstructor: Option[ClassConstructor] = None
  override var isValid: Boolean = true
  var modifiedObject: String = _
  var valueObject: String = _
  var objectInfo: ByteCodeInfo = _
  var valueInfo: ByteCodeInfo = _
}
