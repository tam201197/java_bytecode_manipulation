import scala.collection.mutable.ListBuffer

class ByteCodeInfoOfValueInArray extends ByteCodeInfo{
  var values: ListBuffer[ByteCodeInfo] = new ListBuffer[ByteCodeInfo]()
}
