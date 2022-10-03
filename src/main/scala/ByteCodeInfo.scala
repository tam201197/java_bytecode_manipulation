import org.opalj.br.instructions.Instruction
import scala.collection.mutable.ListBuffer

class ByteCodeInfo {
  var pc: Int = -1
  var instruction: Option[Instruction] = None
  var parametersByteCodeInfo: Option[ListBuffer[ByteCodeInfo]] = None
  var previousByteCodeInfo: Option[ByteCodeInfo] = None
  var multiPrevByteCodeInfo: Option[ListBuffer[ByteCodeInfo]] = None
}
