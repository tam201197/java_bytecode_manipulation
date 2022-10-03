import javassist._
import org.opalj.ai.{AIResult, ValuesDomain}
import org.opalj.ai.domain.{PerformAI, RefineDefUseUsingOrigins}
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{GETFIELD, GETSTATIC, INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, Instruction, LoadClass, LoadClass_W, LoadString, LoadString_W}
import org.opalj.br._
import org.opalj.collection.immutable.Naught
import java.net.URL
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.break


object FirstCode {

  val projectJAR = "C:/Users/tam20/java_bytecode_manipulation/project/project/target/lib/test-dex2jar.jar"
  val project: Project[URL] = Project(new java.io.File(projectJAR))
  val reflectPackageName = "java.lang.reflect"
  var methodCallReflectionInvoke: ListBuffer[Method] = new ListBuffer[Method]()
  var methodCallReflectionSet: ListBuffer[Method] = new ListBuffer[Method]()
  var methodCallReflectionSetAccessible: ListBuffer[Method] = new ListBuffer[Method]()
  var methodCallReflectionTrySetAccessible: ListBuffer[Method] = new ListBuffer[Method]()
  var setAccessibleObjects: ListBuffer[ReflectionUse] = new ListBuffer[ReflectionUse]()

  def setMethodCallReflection(method: Method): Unit = {
    if (method.body.isDefined) {

      var byteCodeInfo = new ByteCodeInfo()
      val body = method.body.get
      val domain = new DefaultDomainWithCFGAndDefUse(project, method) with RefineDefUseUsingOrigins
      val result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]} = PerformAI(domain)
      body.iterate { (pc, instr) =>
        var obj = new ReflectionUse()
        obj.className = method.classFile.fqn
        obj.methodName = method.name
        if (instr.isInvocationInstruction) {
          val name = instr.asInvocationInstruction.name
          val invoke = instr.asMethodInvocationInstruction
          if (checkInstructionCallReflection(instr)) {
            if (name.equals("invoke")) {
              methodCallReflectionInvoke += method
            }
            else if (name.equals("setAccessible")) {
              methodCallReflectionSetAccessible += method
              obj.nameReflectionFunction = Option(name)
              byteCodeInfo.pc = pc
              byteCodeInfo.instruction = Option(instr)
              obj.byteCodeInfo = Option(byteCodeInfo)
              val len = setAccessibleObjects.length
              // if (len == 114)
              //  println(len)
              getInfosFormSetAccessible(pc, body, obj, byteCodeInfo, result)
              setAccessibleObjects += obj
            }
            else if (name.equals("trySetAccessible")) {
              methodCallReflectionTrySetAccessible += method
            }
            else if (name.startsWith("set")) {
              methodCallReflectionSet += method
            }
          }
        }
      }
    }
  }

  def getInfosFormSetAccessible(pc: Integer, body: Code, obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    val operands = result.operandsArray(pc)
    if (operands == null) {
      return
    }
    /*var op = operands.head
    op match {
      case result.domain.IntegerRange(v) =>
        val parameterInfo = new ByteCodeInfo()
        val org = op.PCIndependent
        parameterInfo.pc = Option(org)
        parameterInfo.instruction = Option(body.instructions(org))
        val parametersInfo = new ListBuffer[ByteCodeInfo]
        parametersInfo.append(parameterInfo)
        byteCodeInfo.parametersByteCodeInfo = Option(parametersInfo)
    }*/
    var new_info = new ByteCodeInfo()
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    var op = operands.last
    op match {
      case result.domain.StringValue(s) =>
      case result.domain.DomainReferenceValueTag(v) =>
        if (v.allValues.exists(p =>
          p.upperTypeBound.containsId(ObjectType("java/lang/reflect/Field").id))) {
          result.domain.originsIterator(op).foreach(origin => {
            setFieldToObject(origin, obj, new_info, body, result)
          })
        }
        else if (v.allValues.exists(p =>
          p.upperTypeBound.containsId(ObjectType("java/lang/reflect/Method").id))) {
          result.domain.originsIterator(op).foreach(origin => {
            setMethodToObject(origin, obj, new_info,body, result)
          })
        }
        else if (v.allValues.exists(p =>
          p.upperTypeBound.containsId(ObjectType("java/lang/reflect/Constructor").id))) {
          result.domain.originsIterator(op).foreach(origin => {
            setConstructorToObject(origin, obj, result, body)
          })
        }
      case e ⇒
    }

  }

  def setConstructorToObject(origin: Int, obj: ReflectionUse, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}, body: Code): Unit = {
    if (origin < 0) return
    val operands = result.operandsArray(origin)
    if (operands == null) {
      return
    }
  }

  def setMethodToObject(origin: Integer, obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, body: Code, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    if (origin < 0) {
      obj.description = "There is no result"
      obj.isValid = false
      return
    }
    val instruction = body.instructions(origin)
    byteCodeInfo.pc = origin
    byteCodeInfo.instruction = Option(instruction)
    var methodAndClass = new MethodAndClass()
    var classNames = new ListBuffer[String]()
    var new_info = new ByteCodeInfo()
    instruction.opcode match {
      case GETFIELD.opcode =>
        val invoke = instruction.asInstanceOf[GETFIELD]
        methodAndClass.methodName = invoke.name
        classNames.append(invoke.declaringClass.fqn)
      case GETSTATIC.opcode =>
        val invoke = instruction.asInstanceOf[GETSTATIC]
        methodAndClass.methodName = invoke.name
        classNames.append(invoke.declaringClass.fqn)
      case INVOKEVIRTUAL.opcode |
           INVOKESPECIAL.opcode |
           INVOKESTATIC.opcode |
           INVOKEINTERFACE.opcode =>
        val operands = result.operandsArray(origin)
        if (operands == null) {
          return
        }
        operands.foreach {
          case op@result.domain.StringValue(s) =>
            var lst = new ListBuffer[ByteCodeInfo]()
            var paramInfo = new ByteCodeInfo
            val org = result.domain.origins(op).head
            paramInfo.pc = org
            paramInfo.instruction = Option(body.instructions(org))
            methodAndClass.methodName = s
            lst.append(paramInfo)
            byteCodeInfo.parametersByteCodeInfo = Option(lst)
          case op@result.domain.DomainInitializedArrayValueTag(v) =>
            getMethodParameters(v.origin, origin, v.length.get, methodAndClass, body)
          case op@result.domain.DomainReferenceValueTag(v) =>
            if (v.allValues.exists(p =>
              p.upperTypeBound.containsId(ObjectType.Class.id))) {
              result.domain.originsIterator(op).foreach(org => {
                getClassInfos(org, classNames, obj, new_info, body, result)
                if (!obj.isValid) return
              })
            }
          case op@_ =>
            result.domain.originsIterator(op).foreach(org => {
              setMethodToObject(org, obj, new_info, body, result)
            })
        }
    }
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    if (obj.isValid) {
      methodAndClass.className = classNames
      obj.methodAndClass = Option(methodAndClass)
    }
  }

  def getMethodParameters(start: Integer, end: Integer, count: Integer, obj: MethodAndClass, body: Code ): Unit = {
    val nextPc = body.pcOfNextInstruction(start)
    if (count == 0 || nextPc == end )
      return
    var new_count = count
    val instruction = body.instructions(nextPc)
    instruction match {
      case loadClass: LoadClass =>
        if (loadClass.value.asObjectType.id == ObjectType.Class.id){
          return
        }
        obj.addMethodDescriptors(loadClass.value.asObjectType.fqn)
        new_count = new_count - 1
      case getStatic: GETSTATIC =>
        obj.addMethodDescriptors(getStatic.declaringClass.fqn)
        new_count = new_count - 1
      case _ =>
    }
    getMethodParameters(nextPc, end, new_count, obj, body)
  }

  def setFieldToObject(origin: Integer, obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, body: Code, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    if (origin < 0) {
      obj.description = "There is no result"
      obj.isValid = false
      return
    }
    val instruction = body.instructions(origin)
    byteCodeInfo.pc = origin
    byteCodeInfo.instruction = Option(instruction)
    val operands = result.operandsArray(origin)
    if (operands == null) {
      return
    }

    var new_info = new ByteCodeInfo()
    val fieldAndClass = new FieldAndClass()
    var classNames = new ListBuffer[String]()
    operands.foreach {
      case result.domain.IntegerRange(s) =>
        if (s._1 != s._2) {
          obj.description = "There is no result"
          obj.isValid = false
          return
        }
      case op@result.domain.StringValue(s) =>
        var lst = new ListBuffer[ByteCodeInfo]()
        var paramInfo = new ByteCodeInfo
        val org = result.domain.origins(op).head
        paramInfo.pc = org
        paramInfo.instruction = Option(body.instructions(org))
        fieldAndClass.fieldName = s
        lst.append(paramInfo)
        byteCodeInfo.parametersByteCodeInfo = Option(lst)
      case op@result.domain.DomainReferenceValueTag(v) =>
        v.allValues.foreach(p => {
          if (p.upperTypeBound.containsId(ObjectType.String.id))
            result.domain.originsIterator(op).foreach(org => {
              setFieldToObject(org, obj, new_info, body, result)
              if (obj.getAttribute().isEmpty)
                return
            })
          if (p.upperTypeBound.containsId(ObjectType.Class.id)) {
            result.domain.originsIterator(op).foreach(org => {
              if (fieldAndClass.fieldName == "") return
              getClassInfos(org, classNames, obj, new_info, body, result)
            })
          }
        })
      case _ =>
    }
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    if (obj.isValid) {
      fieldAndClass.className = classNames
      obj.fieldAndClass = Option(fieldAndClass)
    }
  }

  //noinspection DuplicatedCode
  def getClassInfos(origin: Integer, classNames: ListBuffer[String], obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, body: Code, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    if (origin < 0) {
      obj.description = "There is no result"
      obj.isValid = false
      return
    }
    val instruction = body.instructions(origin)
    byteCodeInfo.pc = origin
    byteCodeInfo.instruction = Option(instruction)
    val operands = result.operandsArray(origin)
    if (operands == null) return
    if (operands.equals(Naught)) {
      instruction match {
        case loadClassW: LoadClass_W =>
          val value = loadClassW.value
          classNames.append(value.asObjectType.fqn)
        case loadClass: LoadClass =>
          val value = loadClass.value
          classNames.append(value.asObjectType.fqn)
        case loadString: LoadString =>
          classNames.append(loadString.value)
        case getStatic: GETSTATIC =>
          obj.description = "there is no result. Because class is declared static attribute"
          obj.isValid = false
        case _ =>
      }
    } else {
      operands.foreach {
        case op@result.domain.StringValue(s) =>
          var new_info = new ByteCodeInfo()
          byteCodeInfo.previousByteCodeInfo = Option(new_info)
          val org = result.domain.origins(op).head
          new_info.pc = org
          new_info.instruction = Option(body.instructions(org))
          byteCodeInfo.previousByteCodeInfo = Option(new_info)
          classNames.append(s)
        case op@result.domain.MultipleReferenceValues(v) =>
          var lst = new ListBuffer[ByteCodeInfo]()
          result.domain.originsIterator(op).foreach(org => {
            var new_info = new ByteCodeInfo()
            lst.append(new_info)
            getClassInfos(org, classNames, obj, new_info, body, result)
            byteCodeInfo.multiPrevByteCodeInfo = Option(lst)
          })
        case op@result.domain.DomainReferenceValueTag(v) =>
          result.domain.originsIterator(op).foreach(org => {
            if (instruction.isInvocationInstruction){
              val name = instruction.asMethodInvocationInstruction.name
              if (name == "getClass" && result.domain.origins(operands.head).head < 0)
                return
            }
            var new_info = new ByteCodeInfo()
            byteCodeInfo.previousByteCodeInfo = Option(new_info)
            getClassInfos(org, classNames, obj, new_info, body, result)
          })
        case _ =>
      }
    }
    instruction.opcode match {
      case INVOKEVIRTUAL.opcode |
           INVOKESPECIAL.opcode |
           INVOKESTATIC.opcode |
           INVOKEINTERFACE.opcode =>
        val name = instruction.asMethodInvocationInstruction.name
        if (name == "getSuperclass") {
          if (classNames.isEmpty)
            classNames.append(result.domain.method.classFile.superclassType.get.fqn)
          else {
            val className = classNames.last
            val superClass = project.allClassFiles.filter(p => p.fqn.equals(className)).head.superclassType
            classNames.remove(classNames.length - 1)
            classNames.append(superClass.get.fqn)
          }
        }
      case _ =>
    }
  }


  def checkInstructionCallReflection(instruction: Instruction): Boolean = {
    instruction.opcode match {
      case INVOKEVIRTUAL.opcode |
           INVOKESPECIAL.opcode |
           INVOKESTATIC.opcode |
           INVOKEINTERFACE.opcode =>
        val invoke = instruction.asMethodInvocationInstruction
        checkReflection(invoke.declaringClass.toJava)
      case _ => false
    }
  }

  def checkReflection(info: String): Boolean = {
    info.startsWith(reflectPackageName) || info.equals("java.lang.Class")
  }

  def isMethodUsingReflection(method: Method): Boolean = {
    if (method.body.isDefined) {
      val code = method.body.get
      val i_invoke = code.instructions.filter(instr => instr != null && instr.isInvocationInstruction)
      for (instr <- i_invoke) {
        if (checkInstructionCallReflection(instr)) {
          return true
        }
      }
      false
    }
    else false
  }

  def workWithReflectionSetAccessible(method: Method): Unit = {
    val domain = new DefaultDomainWithCFGAndDefUse(project, method) with RefineDefUseUsingOrigins
    val body = method.body.get
    lazy val result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]} = PerformAI(domain)
    body.iterate { (pc, instruction) =>
      instruction.opcode match {
        case INVOKEVIRTUAL.opcode |
             INVOKESPECIAL.opcode |
             INVOKEINTERFACE.opcode =>
          val invoke = instruction.asMethodInvocationInstruction
          val operands = result.operandsArray(pc)
          val params = invoke.methodDescriptor.parameterTypes
          val index = params.size
          if (operands == null)
            return
        // val op = operands(index)
        case INVOKESTATIC.opcode =>
        case _ =>
      }

    }

  }

  def main(args: Array[String]): Unit = {
    val methods_with_body = project.allMethodsWithBody
    val method_using_reflection = methods_with_body.filter(method => isMethodUsingReflection(method))
    println(method_using_reflection.length)
    methods_with_body.foreach(method => {
      setMethodCallReflection(method)
    })
    println("invoke: " + methodCallReflectionInvoke.length)
    println("setAccessible: " + methodCallReflectionSetAccessible.length)
    println("set: " + methodCallReflectionSet.length)
    println("trySetAccessible: " + methodCallReflectionTrySetAccessible.length)
    methodCallReflectionInvoke.foreach(method => {
      workWithReflectionSetAccessible(method)
    })

    /*val pool = ClassPool.getDefault
    pool.insertClassPath(projectJAR)
    method_using_reflection.foreach(method => {
      val qualified_name = method.classFile.fqn.replace('/', '.')
      val cc = pool.get(qualified_name)
      try {
        var ct_method = cc.getDeclaredMethod(method.name)
        count += 1
        ct_method.setModifiers(Modifier.PUBLIC)
      } catch{
        case e: NotFoundException =>
      }
    }
    )
    println(count)*/
  }
}
