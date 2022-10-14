import scala.collection.mutable.ListBuffer

class MultiByteCodeInfo extends ByteCodeInfo {
  var values: ListBuffer[ByteCodeInfo] = new ListBuffer[ByteCodeInfo]()
}
