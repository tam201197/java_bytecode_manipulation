import javassist._
import org.opalj.ai.{AIResult, ValueOriginsIterator, ValuesDomain}
import org.opalj.ai.domain.{PerformAI, RefineDefUseUsingOrigins}
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.br.analyses.Project
import org.opalj.br.instructions._
import org.opalj.br._
import org.opalj.value.AnIntegerValue
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
  var setAccessibleObjects: ListBuffer[ReflectionUse] = new ListBuffer[ReflectionUse]()
  var setObjects: ListBuffer[ReflectionSet] = new ListBuffer[ReflectionSet]()
  var count = 0

  def setMethodCallReflection(method: Method): Unit = {
    if (method.body.isDefined) {
      val body = method.body.get
      val domain = new DefaultDomainWithCFGAndDefUse(project, method) with RefineDefUseUsingOrigins
      val result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]} = PerformAI(domain)
      body.iterate { (pc, instr) =>
        if (instr.isInvocationInstruction && checkInstructionCallReflection(instr)) {
          var byteCodeInfo = new ByteCodeInfo()
          val name = instr.asInvocationInstruction.name
          if (name.equals("invoke")) {
            var obj = new ReflectionSet()
            makeReflectionUseObject(pc, instr, method, obj, byteCodeInfo)
            methodCallReflectionInvoke += method
            getInfosFromInvoke(pc, body, obj, byteCodeInfo, result)
          }
          else if (name.equals("setAccessible")) {
            methodCallReflectionSetAccessible += method
            var obj = new ReflectionSetAccessible()
            makeReflectionUseObject(pc, instr, method, obj, byteCodeInfo)
            val len = setAccessibleObjects.length
            if (len == 7)
              println(len)
            getInfosFromSetAccessible(pc, body, obj, byteCodeInfo, result)
            setAccessibleObjects += obj
          }
          else if (name.startsWith("set")) {
            var obj = new ReflectionSet()
            makeReflectionUseObject(pc, instr, method, obj, byteCodeInfo)
            methodCallReflectionSet += method
            /*getInfosFromSet(pc, body, obj, byteCodeInfo, result)
            setObjects += obj
            count += 1
            println(count)*/
          }

        }
      }
    }
  }

  def makeReflectionUseObject(pc: Int, instruction: Instruction, method: Method, obj: ReflectionUse, byteCodeInfo: ByteCodeInfo): Unit = {
    obj.className = method.classFile.fqn
    obj.method = method
    val name = instruction.asInvocationInstruction.name
    obj.nameReflectionFunction = Option(name)
    byteCodeInfo.setInfo(pc, instruction)
    obj.byteCodeInfo = Option(byteCodeInfo)
  }


  def getInfosFromSetAccessible(pc: Integer, body: Code, obj: ReflectionSetAccessible, byteCodeInfo: ByteCodeInfo, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
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
            setMethodToObject(origin, obj, new_info, body, result)
          })
        }
        else if (v.allValues.exists(p =>
          p.upperTypeBound.containsId(ObjectType("java/lang/reflect/Constructor").id))) {
          result.domain.originsIterator(op).foreach(origin => {
            setConstructorToObject(origin, obj, new_info, body, result)
          })
        }
      case e ⇒
    }
  }

  def getInfosFromInvoke(pc: Integer, body: Code, obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    val operands = result.operandsArray(pc)
    if (operands == null) {
      return
    }
    var new_info = new ByteCodeInfo()
    var methodAndClass = new MethodAndClass()
    var classNames = new ListBuffer[String]()
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    /*operands.foreach{
      case result.domain.DomainInitializedArrayValueTag(v) =>
        //getMethodParameters(v.origin, pc, v.length.get, methodAndClass, body)
      case op@result.domain.DomainReferenceValueTag(v) =>
        if (v.allValues.exists(p =>
          p.upperTypeBound.containsId(ObjectType.Class.id))) {
          result.domain.originsIterator(op).foreach(org => {
            getClassInfos(org, classNames, obj, new_info, body, result)
            if (!obj.isValid) return
          })
        }
      case _ =>
    }*/
  }

  def getInfosFromSet(pc: Integer, body: Code, obj: ReflectionSet, byteCodeInfo: ByteCodeInfo, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    val operands = result.operandsArray(pc)
    if (operands == null) {
      return
    }
    var new_info = new ByteCodeInfo()
    var methodAndClass = new MethodAndClass()
    var classNames = new ListBuffer[String]()
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    val value_operand = operands.head
    value_operand match {
      case result.domain.IntegerRange(v) =>
        println(v.toString())
      case _ =>
        if (value_operand.isInstanceOf[result.domain.AnIntegerValue]) {
          obj.isValid = false
          return
        }
        getValueParameterInReflectionSet(result.domain.originsIterator(value_operand), body, obj, result)
    }
    val object_operand = operands(1)
    var object_origins = result.domain.originsIterator(operands(1))

    getObjectParameterInReflectionSet(object_origins, body, obj, new_info, result)
    result.domain.originsIterator(operands.last).foreach(org => {
      val instr = body.instructions(org)
      if (instr.isInvocationInstruction &&
        (instr.asInvocationInstruction.name.equals("getDeclaredField") || instr.asInvocationInstruction.name.equals("getField")))
        setFieldToObject(org, obj, new_info, body, result)
    }
    )
  }

  def getValueParameterInReflectionSet(origins: ValueOriginsIterator, body: Code, obj: ReflectionSet,
                                       result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    origins.foreach(org => {
      checkOriginValue(org, obj)
      val operands = result.operandsArray(org)
      val instruction = body.instructions(org)
      val new_info = new ByteCodeInfo()
      new_info.setInfo(org, instruction)
      obj.valueInfo.append(new_info)
      instruction.opcode match {
        case ACONST_NULL.opcode =>
          obj.valueObject.append("null")
        case INVOKESTATIC.opcode =>
          val invoke = instruction.asMethodInvocationInstruction
          val class_name = invoke.declaringClass.toJava
          val method_name = invoke.name
          if (class_name.equals(obj.className))
            obj.valueObject.append("this." + method_name)
          else
            obj.valueObject.append(class_name + "." + method_name)
        case _ =>
      }
    })
  }

  def checkOriginValue(origin: Int, obj: ReflectionSet): Unit = {
    if (origin < 0) {
      if (origin == -1 && !obj.method.isStatic) {
        obj.modifiedObject.append("this")
      } else
        obj.isValid = false
    }
  }

  def getObjectParameterInReflectionSet(origins: ValueOriginsIterator, body: Code, obj: ReflectionSet, byteCodeInfo: ByteCodeInfo,
                                        result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    origins.foreach(org => {
      checkOriginValue(org, obj)
      val operands = result.operandsArray(org)
      var instruction = body.instructions(org)
      var new_info = new ByteCodeInfo()
      new_info.setInfo(org, instruction)
      obj.objectInfo.append(new_info)
      instruction.opcode match {
        case ACONST_NULL.opcode =>
          obj.modifiedObject.append("null")
        case _ =>
      }
      /*result.domain.originsIterator(operands.last).foreach(org =>{
        instruction = body.instructions(org)
        if (instruction.isInvocationInstruction && instruction.asMethodInvocationInstruction.name.equals("getDeclaredField")) {
          new_info = new ByteCodeInfo
          new_info.pc = org
          new_info.instruction = Option(instruction)
          setFieldToObject(org, obj, new_info, body, result)
        }
      })*/
    })
  }


  def setConstructorToObject(origin: Int, obj: ReflectionSetAccessible, byteCodeInfo: ByteCodeInfo, body: Code, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    if (origin < 0) return
    val operands = result.operandsArray(origin)
    if (operands == null) {
      return
    }
    val instruction = body.instructions(origin)
    byteCodeInfo.setInfo(origin, instruction)
    var classNames = new ListBuffer[String]()
    var new_info = new ByteCodeInfo()
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    var op = operands.head
    var classConstructor = new ClassConstructor()
    op match {
      case result.domain.DomainInitializedArrayValueTag(v) =>
        val array_length = v.length.get
        if (array_length > 0) {
          var param_list = new ListBuffer[ByteCodeInfo]()
          byteCodeInfo.parametersByteCodeInfo = Option(param_list)
          getMethodParameters(v.origin, origin, array_length, classConstructor, byteCodeInfo, body)
        }
      case _ =>
    }
    if (!obj.isValid) return
    result.domain.originsIterator(operands.last).foreach(org => {
      getClassInfos(org, classNames, obj, new_info, body, result)
      if (!obj.isValid) return
    })
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    if (obj.isValid) {
      classConstructor.className = classNames
      obj.classConstructor = Option(classConstructor)
    }
  }

  def setMethodToObject(origin: Integer, obj: ReflectionSetAccessible, byteCodeInfo: ByteCodeInfo, body: Code, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    byteCodeInfo.pc = origin
    if (origin < 0) {
      obj.isValid = false
      return
    }
    val instruction = body.instructions(origin)
    byteCodeInfo.instruction = instruction
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
            paramInfo.setInfo(org, body.instructions(org))
            methodAndClass.methodName = s
            lst.append(paramInfo)
            byteCodeInfo.parametersByteCodeInfo = Option(lst)
          case op@result.domain.DomainInitializedArrayValueTag(v) =>
            val param_length = v.length.get
            if (param_length > 0) {
              var lst = new ListBuffer[ByteCodeInfo]()
              byteCodeInfo.parametersByteCodeInfo = Option(lst)
              getMethodParameters(v.origin, origin, v.length.get, methodAndClass, byteCodeInfo, body)
            }

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

  def getValuesOfParameters(start: Int, end: Int, obj: MethodAndClass, body: Code): Unit = {
    val nextPc = body.pcOfNextInstruction(start)
    if (nextPc == end)
      return
    val instruction = body.instructions(nextPc)


  }

  def getMethodParameters(start: Integer, end: Integer, count: Integer, obj: ObjectCalledByReflection, byteCodeInfo: ByteCodeInfo, body: Code): Unit = {
    val nextPc = body.pcOfNextInstruction(start)
    if (count == 0 || nextPc == end)
      return
    var new_count = count
    val instruction = body.instructions(nextPc)
    instruction match {
      case loadClass: LoadClass =>
        if (loadClass.value.asObjectType.id == ObjectType.Class.id) {
          return
        }
        val paramType = loadClass.value.asObjectType.fqn
        saveParametersInfoInByteCodeInfo(nextPc, paramType, instruction, obj, byteCodeInfo)
        new_count = new_count - 1
      case getStatic: GETSTATIC =>
        val paramType = getStatic.declaringClass.fqn
        saveParametersInfoInByteCodeInfo(nextPc, paramType, instruction, obj, byteCodeInfo)
        new_count = new_count - 1
      case _ =>
    }
    getMethodParameters(nextPc, end, new_count, obj, byteCodeInfo, body)
  }

  def saveParametersInfoInByteCodeInfo(pc: Int, param: String, instruction: Instruction, obj: ObjectCalledByReflection, byteCodeInfo: ByteCodeInfo): Unit = {
    obj.addParametersPC(pc)
    obj.addParametersType(param)
    val new_info = new ByteCodeInfo()
    new_info.setInfo(pc, instruction)
    byteCodeInfo.parametersByteCodeInfo.get.append(new_info)
  }

  def setFieldToObject(origin: Integer, obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, body: Code, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    byteCodeInfo.pc = origin
    if (origin < 0) {
      obj.isValid = false
      return
    }
    val instruction = body.instructions(origin)
    byteCodeInfo.instruction = instruction
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
          obj.isValid = false
          return
        }
      case op@result.domain.StringValue(s) =>
        var lst = new ListBuffer[ByteCodeInfo]()
        var paramInfo = new ByteCodeInfo
        val org = result.domain.origins(op).head
        paramInfo.setInfo(org, body.instructions(org))
        fieldAndClass.fieldName = s
        lst.append(paramInfo)
        byteCodeInfo.parametersByteCodeInfo = Option(lst)
      case op@result.domain.DomainReferenceValueTag(v) =>
        v.allValues.foreach(p => {
          if (p.upperTypeBound.containsId(ObjectType.String.id))
            result.domain.originsIterator(op).foreach(org => {
              setFieldToObject(org, obj, new_info, body, result)
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

  def getClassInfos(origin: Integer, classNames: ListBuffer[String], obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, body: Code,
                    result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    byteCodeInfo.pc = origin
    if (origin < 0) {
      if (origin == -1 && !obj.method.isStatic)
        classNames.append(obj.className)
      else
        obj.isValid = false
      return
    }
    val instruction = body.instructions(origin)
    byteCodeInfo.instruction = instruction
    val operands = result.operandsArray(origin)
    if (operands == null) return
    instruction match {
      case loadClassW: LoadClass_W =>
        val value = loadClassW.value
        classNames.append(value.asObjectType.fqn)
      case loadClass: LoadClass =>
        val value = loadClass.value
        classNames.append(value.asObjectType.fqn)
      case _ =>
    }
    instruction.opcode match {
      case INVOKEVIRTUAL.opcode |
           INVOKESPECIAL.opcode |
           INVOKESTATIC.opcode |
           INVOKEINTERFACE.opcode =>
        val methodName = instruction.asInvocationInstruction.name
        if (methodName == "getSuperclass" || methodName == "asSubclass") {
          if (!operands.head.equals(operands.last)){
            var lst = new ListBuffer[ByteCodeInfo]()
            var param_info = new ByteCodeInfo()
            lst.append(param_info)
            byteCodeInfo.parametersByteCodeInfo = Option(lst)
            result.domain.originsIterator(operands.head).foreach(org =>
              getClassInfos(org, classNames, obj, param_info, body, result)
            )
            var new_info = new ByteCodeInfo()
            byteCodeInfo.previousByteCodeInfo = Option(new_info)
            result.domain.originsIterator(operands.last).foreach(org =>
              getAnotherInfos(org, obj, new_info, body, result)
            )
          } else {
            var new_info = new ByteCodeInfo()
            byteCodeInfo.previousByteCodeInfo = Option(new_info)
            var new_classNames = new ListBuffer[String]()
            result.domain.originsIterator(operands.last).foreach(org => {
              getClassInfos(org, new_classNames, obj, new_info, body, result)
              new_classNames.foreach(name =>
                project.allClassFiles.filter(class_file => class_file.fqn.equals(name)).foreach(class_file =>
                  if (class_file.superclassType.isDefined)
                    classNames.append(class_file.superclassType.get.fqn)
                  else
                    obj.isValid = false
                )
              )
            })
          }
        } else {
          var lst = new ListBuffer[ByteCodeInfo]()
          operands.foreach(op => {
            if (!obj.isValid) return
            if (methodName == "forName" || (!op.equals(operands.last) && !operands.head.equals(operands.last))) {
              var param_info = new ByteCodeInfo()
              byteCodeInfo.parametersByteCodeInfo = Option(lst)
              op match {
                case result.domain.StringValue(s) =>
                  val org = result.domain.origins(op).head
                  param_info.setInfo(org, body.instructions(org))
                  classNames.append(s)
                  lst.append(param_info)
                case result.domain.MultipleReferenceValues(v) =>
                  var multi_param = new MultiByteCodeInfo()
                  lst.append(multi_param)
                  if (v.filter(p => p.upperTypeBound.containsId(ObjectType.String.id)).nonEmpty) {
                    result.domain.originsIterator(op).foreach(org => {
                      var new_info = new ByteCodeInfo()
                      multi_param.values.append(new_info)
                      getClassName(org, classNames, obj, new_info, body, result)
                    })
                  } else {
                    result.domain.originsIterator(op).foreach(org => {
                      var new_info = new ByteCodeInfo()
                      multi_param.values.append(new_info)
                      getAnotherInfos(org, obj, new_info, body, result)
                    })
                  }
                case result.domain.DomainReferenceValueTag(v) =>
                    lst.append(param_info)
                    result.domain.originsIterator(op).foreach(org => {
                      getAnotherInfos(org, obj, param_info, body, result)
                    })
                case _ =>
              }
            } else {
              var lst = new ListBuffer[ByteCodeInfo]()
              var new_info = new ByteCodeInfo()
              byteCodeInfo.previousByteCodeInfo = Option(new_info)
              if (classNames.isEmpty) {
                result.domain.originsIterator(op).foreach(org => {
                  getClassInfos(org, classNames, obj, new_info, body, result)
                })
              } else {
                result.domain.originsIterator(op).foreach(org => {
                  getAnotherInfos(org, obj, new_info, body, result)
                })
              }
            }

          })
        }
      case _ =>
    }
}

def getAnotherInfos(org: Int, obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, body: Code,
                    result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
  byteCodeInfo.pc = org
  if (org < 0) return
  val instruction = body.instructions(org)
  byteCodeInfo.instruction = instruction
  val operands = result.operandsArray(org)
  if (operands == null) return
  var lst = new ListBuffer[ByteCodeInfo]()
  operands.foreach(op =>
    if (!op.equals(operands.last)) {
      var param_info = new ByteCodeInfo()
      lst.append(param_info)
      op match {
        case result.domain.StringValue(s) =>
          val org = result.domain.origins(op).head
          param_info.setInfo(org, body.instructions(org))
        case result.domain.MultipleReferenceValues(v) =>
          var multi_param = new MultiByteCodeInfo()
          result.domain.originsIterator(op).foreach(org => {
            var new_info = new ByteCodeInfo()
            multi_param.values.append(new_info)
            getAnotherInfos(org, obj, new_info, body, result)
          })
        case result.domain.DomainReferenceValueTag(v) =>
          result.domain.originsIterator(op).foreach(org => {
            getAnotherInfos(org, obj, param_info, body, result)
          })
        case _ =>
      }
    } else {
      var new_info = new ByteCodeInfo()
      byteCodeInfo.previousByteCodeInfo = Option(new_info)
      result.domain.originsIterator(op).foreach(org => {
        getAnotherInfos(org, obj, new_info, body, result)
      })
    }
  )
}

def getClassName(org: Int, classNames: ListBuffer[String], obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, body: Code,
                 result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
  byteCodeInfo.pc = org
  if (org < 0) {
    obj.isValid = false
    return
  }
  val instruction = body.instructions(org)
  byteCodeInfo.instruction = instruction
  val operands = result.operandsArray(org)
  if (operands == null) return
  if (operands.equals(Naught)) {
    instruction match {
      case loadString: LoadString =>
        classNames.append(loadString.value)
      case _ =>
    }
  }
}

/*  def getClassInfos(origin: Integer, classNames: ListBuffer[String], obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, body: Code, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    byteCodeInfo.pc = origin
    if (origin < 0) {
      if (origin != -1 || obj.method.isStatic) {
        obj.isValid = false
      }
      return
    }
    val instruction = body.instructions(origin)
    byteCodeInfo.instruction = instruction
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
          obj.isValid = false
        case _ =>
      }
    } else {
      operands.foreach( op => {
        if (!obj.isValid)
          return
        op match {
          case result.domain.StringValue(s) =>
            var new_info = new ByteCodeInfo()
            byteCodeInfo.previousByteCodeInfo = Option(new_info)
            val org = result.domain.origins(op).head
            new_info.setInfo(org, body.instructions(org))
            byteCodeInfo.previousByteCodeInfo = Option(new_info)
            classNames.append(s)
          case result.domain.MultipleReferenceValues(v) =>
            var lst = new ListBuffer[ByteCodeInfo]()
            result.domain.originsIterator(op).foreach(org => {
              var new_info = new ByteCodeInfo()
              lst.append(new_info)
              getClassInfos(org, classNames, obj, new_info, body, result)
              byteCodeInfo.multiPrevByteCodeInfo = Option(lst)
            })
          case result.domain.DomainReferenceValueTag(v) =>
            result.domain.originsIterator(op).foreach(org => {
              if (instruction.isInvocationInstruction) {
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
      })
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
  }*/


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
