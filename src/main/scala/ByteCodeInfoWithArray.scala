import scala.collection.mutable.ListBuffer

class ByteCodeInfoWithArray extends ByteCodeInfo {
  var values: ListBuffer[ByteCodeInfo] = new ListBuffer[ByteCodeInfo]()
}
