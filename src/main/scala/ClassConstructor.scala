import scala.collection.mutable.ListBuffer

class ClassConstructor extends ObjectCalledByReflection {
  override var className: ListBuffer[String] = new ListBuffer[String]()
  override var parametersType: ListBuffer[String] = new ListBuffer[String]()
  override var parametersPC: ListBuffer[Int] = new ListBuffer[Int]()

  override def addParametersType(methodDescriptor: String): Unit = {
    this.parametersType.append(methodDescriptor)
  }

  override def addParametersPC(pc: Int): Unit = {
    this.parametersPC.append(pc)
  }
}
