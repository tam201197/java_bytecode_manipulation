import org.opalj.br._

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import org.opalj.br._


class ReflectionUse {
  var className: String = ""
  var methodName: String = ""
  var nameReflectionFunction : Option[String] = None
  var byteCodeInfo: Option[ByteCodeInfo] = None
  var parameters: ListBuffer[ReflectionUse] = new ListBuffer[ReflectionUse]()
  var description: String = ""
  var attribute: Map[String, ListBuffer[String]] = Map()
  var methodDescriptors: ListBuffer[String] = new ListBuffer[String]()
  var constructors: Map[String, ListBuffer[String]] = Map()
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
  def setAttributeName(attributeName: String, classNames: ListBuffer[String]): Unit = {
    this.attribute.put(attributeName, classNames)
  }
   def setMethodName(methodName: String): Unit = {
     this.methodName = methodName
   }

  def setConstructor(className: String, parameters: ListBuffer[String]): Unit = {
    this.constructors.put(className, parameters)
  }

  def getAttribute(): Map[String, ListBuffer[String]] = {
    this.attribute
  }

}
