import scala.collection.mutable.ListBuffer

class MethodAndClass extends ObjectCalledByReflection {
  var methodName: String = ""
  override var className: ListBuffer[String] = new ListBuffer[String]()
  override var parametersType: ListBuffer[String] = new ListBuffer[String]()
  var parametersValues: ListBuffer[String] = new ListBuffer[String]()
  override var parametersPC: ListBuffer[Int] = new ListBuffer[Int]()
  var isStatic: Option[Boolean] = None

  override def addParametersType(methodDescriptor: String): Unit = {
    this.parametersType.append(methodDescriptor)
  }

  override def addParametersPC(pc: Int): Unit = {
    this.parametersPC.append(pc)
  }
}
