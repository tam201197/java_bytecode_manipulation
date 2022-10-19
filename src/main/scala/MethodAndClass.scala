import scala.collection.mutable.ListBuffer

class MethodAndClass extends ObjectCalledByReflection {
  var methodName: String = _
  override var className: String = _
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
