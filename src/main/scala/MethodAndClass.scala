import scala.collection.mutable.ListBuffer

class MethodAndClass {
  var methodName: String = ""
  var className: ListBuffer[String] = new ListBuffer[String]()
  var methodDescriptors: ListBuffer[String] = new ListBuffer[String]()
  var parametersValues: ListBuffer[String] = new ListBuffer[String]()
  var isStatic: Option[Boolean] = None
  def addMethodDescriptors(methodDescriptor: String): Unit = {
    this.methodDescriptors.append(methodDescriptor)
  }
}
