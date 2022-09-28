import org.opalj.br._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

class ReflectionUse {
  private var className: String = ""
  private var attribute: Map[String, ListBuffer[String]] =Map()
  private var methodName: String = ""
  private var methodDescriptors: ListBuffer[String] = new ListBuffer[String]()
  private var constructors: Map[String, ListBuffer[String]] = Map()
  // pc, instruction, method, class mit fqn

  def setClassName(className: String): Unit = {
    this.className = className
  }
  def setAttributeName(attributeName: String, classNames: ListBuffer[String]): Unit = {
    this.attribute.put(attributeName, classNames)
  }
   def setMethodName(methodName: String): Unit = {
     this.methodName = methodName
   }
  def addMethodDescriptors(methodDescriptor: String): Unit = {
    this.methodDescriptors.append(methodDescriptor)
  }

  def setConstructor(className: String, parameters: ListBuffer[String]): Unit = {
    this.constructors.put(className, parameters)
  }

  def getAttribute(): Map[String, ListBuffer[String]] = {
    this.attribute
  }

}
