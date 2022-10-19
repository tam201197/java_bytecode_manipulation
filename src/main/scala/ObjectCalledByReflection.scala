import scala.collection.mutable.ListBuffer

trait ObjectCalledByReflection {
  var className: String
  var parametersType: ListBuffer[String]
  var parametersPC: ListBuffer[Int]

  def addParametersType(methodDescriptor: String): Unit
  def addParametersPC(pc: Int): Unit
}
