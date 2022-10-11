import org.opalj.br._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map


class ReflectionSetAccessible extends ReflectionUse{
  var className: String = ""
  var method: Method = _
  var nameReflectionFunction : Option[String] = None
  var byteCodeInfo: Option[ByteCodeInfo] = None
  var methodDescriptors: ListBuffer[String] = new ListBuffer[String]()
  var fieldAndClass: Option[FieldAndClass] = None
  var methodAndClass: Option[MethodAndClass] = None
  var classConstructor: Option[ClassConstructor] = None
  var isValid: Boolean = true

  def setClassName(className: String): Unit = {
    if (methodAndClass.isDefined){
      methodAndClass.get.className.append(className)
    } else if (fieldAndClass.isDefined){
      fieldAndClass.get.className.append(className)
    } else if (classConstructor.isDefined) {
      classConstructor.get.className.append(className)
    }
  }


}
