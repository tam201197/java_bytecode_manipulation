import javassist._
import org.opalj.ai.{AIResult, ValueOriginsIterator, ValuesDomain}
import org.opalj.ai.domain.{PerformAI, RefineDefUseUsingOrigins}
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.br.analyses.Project
import org.opalj.br.instructions._
import org.opalj.br._
import org.opalj.collection.immutable.Naught

import java.net.URL
import scala.collection.mutable.ListBuffer


object FirstCode {

  val projectJAR = "C:/Users/tam20/java_bytecode_manipulation/project/project/target/lib/test-dex2jar.jar"
  val project: Project[URL] = Project(new java.io.File(projectJAR))
  val reflectPackageName = "java.lang.reflect"
  var methodCallReflectionInvoke: ListBuffer[Method] = new ListBuffer[Method]()
  var methodCallReflectionSet: ListBuffer[Method] = new ListBuffer[Method]()
  var methodCallReflectionSetAccessible: ListBuffer[Method] = new ListBuffer[Method]()
  var setAccessibleObjects: ListBuffer[ReflectionSetAccessible] = new ListBuffer[ReflectionSetAccessible]()
  var setObjects: ListBuffer[ReflectionSet] = new ListBuffer[ReflectionSet]()
  var invokeObjects: ListBuffer[ReflectionInvoke] = new ListBuffer[ReflectionInvoke]()

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
            var obj = new ReflectionInvoke()
            makeReflectionUseObject(pc, instr, method, obj, byteCodeInfo)
            methodCallReflectionInvoke += method
            //if (invokeObjects.length == 187)
            //  println(invokeObjects.length)
            getInfosFromInvoke(pc, body, obj, byteCodeInfo, result)
            invokeObjects += obj
          }
          else if (name.equals("setAccessible")) {
            methodCallReflectionSetAccessible += method
            var obj = new ReflectionSetAccessible()
            makeReflectionUseObject(pc, instr, method, obj, byteCodeInfo)
            getInfosFromSetAccessible(pc, body, obj, byteCodeInfo, result)
            setAccessibleObjects += obj
          }
          else if (name.startsWith("set")) {
            var obj = new ReflectionSet()
            makeReflectionUseObject(pc, instr, method, obj, byteCodeInfo)
            methodCallReflectionSet += method
            getInfosFromSet(pc, body, obj, byteCodeInfo, result)
            setObjects += obj
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
    var new_info = new ByteCodeInfo()
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    var op = operands.last
    op match {
      case result.domain.MultipleReferenceValues(v) =>
        obj.isValid = false
        return
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
    result.domain.originsIterator(operands.last).foreach(org => {
      setMethodToObject(org, obj, new_info, body, result)
      if (!obj.isValid) return
    })
    var op = operands.head
    op match {
      case result.domain.DomainInitializedArrayValueTag(v) =>
        var list_params = new ListBuffer[String]()
        /*getValuesOfParameters(v.origin, pc, obj, list_params, body, result)
        if (obj.isValid){
          obj.asInstanceOf[ReflectionInvoke].valueObjects = list_params
        }*/
      case result.domain.MultipleReferenceValues(v) =>
        obj.isValid = false
        return
      case result.domain.DomainReferenceValueTag(v) =>
        result.domain.originsIterator(op).foreach(org => {
          var info = new StringBuilder()
          getParameterInReflectionFunction(org, body, obj, new_info, info, result)
          if (obj.isValid){
            obj.asInstanceOf[ReflectionInvoke].valueObjects.append(info.toString())
            obj.asInstanceOf[ReflectionInvoke].valueInfo.append(new_info)
          }
        })
      case _ =>
    }
    var iter = operands.toIterator
    while (iter.hasNext && !op.equals(operands.last)){
      op = iter.next()
      op match {
        case result.domain.MultipleReferenceValues(v) =>
          obj.isValid = false
          return
        case result.domain.DomainReferenceValueTag(v) =>
          result.domain.originsIterator(op).foreach(org => {
            var info = new StringBuilder()
            getParameterInReflectionFunction(org, body, obj, new_info, info, result)
            if (obj.isValid){
              obj.asInstanceOf[ReflectionInvoke].modifiedObject = info.toString()
              obj.asInstanceOf[ReflectionInvoke].objectInfo = new_info
            }
          })
        case _ =>
      }
    }

  }

  def getInfosFromSet(pc: Integer, body: Code, obj: ReflectionSet, byteCodeInfo: ByteCodeInfo, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    val operands = result.operandsArray(pc)
    if (operands == null) {
      return
    }
    var new_info = new ByteCodeInfo()
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    val value_operand = operands.head
    var lst = new ListBuffer[ByteCodeInfo]()
    byteCodeInfo.parametersByteCodeInfo = Option(lst)
    var value_param = new ByteCodeInfo()
    lst.append(value_param)
    var info = new StringBuilder()
    value_operand match {
      case result.domain.IntegerRange(v) =>
        println(v.toString())
      case result.domain.MultipleReferenceValues(v) =>
        obj.isValid = false
        return
      case result.domain.DomainReferenceValueTag(v) =>
        result.domain.originsIterator(value_operand).foreach(org => {
          getParameterInReflectionFunction(org, body, obj, value_param, info, result)
          obj.valueObject = info.toString()
          obj.valueInfo = value_param
        })
      case _ =>
        if (value_operand.isInstanceOf[result.domain.AnIntegerValue]) {
          obj.isValid = false
          return
        }
    }
    var object_param = new ByteCodeInfo()
    lst.append(object_param)
    var object_operand = operands(1)
    info = new StringBuilder()
    object_operand match {
      case result.domain.IntegerRange(v) =>
        println(v.toString())
      case result.domain.MultipleReferenceValues(v) =>
        obj.isValid = false
        return
      case result.domain.DomainReferenceValueTag(v) =>
        result.domain.originsIterator(value_operand).foreach(org => {
          getParameterInReflectionFunction(org, body, obj, object_param, info, result)
          obj.modifiedObject = info.toString()
          obj.objectInfo = object_param
        })
      case _ =>
        if (value_operand.isInstanceOf[result.domain.AnIntegerValue]) {
          obj.isValid = false
          return
        }
    }
    result.domain.originsIterator(operands.last).foreach(org => {
      if (org < 0) {
        obj.isValid = false
        return
      }
      val instr = body.instructions(org)
      if (instr.isInvocationInstruction &&
        (instr.asInvocationInstruction.name.equals("getDeclaredField") || instr.asInvocationInstruction.name.equals("getField")))
        setFieldToObject(org, obj, new_info, body, result)
    }
    )
  }

  def getParameterInReflectionFunction(origin: Int, body: Code, obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, info: StringBuilder,
                                            result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    byteCodeInfo.pc = origin
    if (origin < 0) {
      checkOriginValue(origin, obj, info)
      return
    }
    val instruction = body.instructions(origin)
    byteCodeInfo.instruction = instruction
    instruction.opcode match {
      case ACONST_NULL.opcode =>
        info.append("null")
      case INVOKESTATIC.opcode =>
        val invoke = instruction.asMethodInvocationInstruction
        val class_name = invoke.declaringClass.toJava
        val method_name = invoke.name
        if (class_name.equals(obj.className))
          info.append("this." + method_name)
        else {
          info.append(class_name.split('.').last + "." + method_name)
        }
      case INVOKEVIRTUAL.opcode |
           INVOKESPECIAL.opcode |
           INVOKEINTERFACE.opcode =>
        var valueObject = new StringBuilder()
        objectToString(origin, valueObject, obj, byteCodeInfo, body, result)
        if (obj.isValid)
          info.append(valueObject.toString())
      case GETFIELD.opcode =>
        val get_field = instruction.asInstanceOf[GETFIELD]
        info.append("this." + get_field.name)
      case GETSTATIC.opcode =>
        val get_static = instruction.asInstanceOf[GETSTATIC]
        info.append("this." + get_static.name)
      case NEW.opcode =>
        val instruction_new = instruction.asInstanceOf[NEW]
        info.append("new " + instruction_new.objectType.simpleName + "()")
      case _ =>
        obj.isValid = false
    }
  }

  def checkOriginValue(origin: Int, obj: ReflectionUse, info: StringBuilder): Unit = {
    if (origin == -1 && !obj.method.isStatic) {
      info.append("this")
    } else
      obj.isValid = false
  }

  def getObjectParameterInReflectionFunction(origin: Int, body: Code, obj: ReflectionSet, byteCodeInfo: ByteCodeInfo,
                                             result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    byteCodeInfo.pc = origin
    if (origin < 0) {
      return
    }
    var instruction = body.instructions(origin)
    byteCodeInfo.instruction = instruction
    var new_info = new ByteCodeInfo()
    new_info.setInfo(origin, instruction)
    obj.objectInfo = new_info
    instruction.opcode match {
      case ACONST_NULL.opcode =>
        obj.modifiedObject = "null"
      case GETFIELD.opcode =>
        val get_field = instruction.asInstanceOf[GETFIELD]
        obj.modifiedObject = "this." + get_field.name
      case GETSTATIC.opcode =>
        val get_static = instruction.asInstanceOf[GETSTATIC]
        obj.modifiedObject = "this." + get_static.name
      case INVOKESTATIC.opcode =>
        val invoke = instruction.asMethodInvocationInstruction
        val class_name = invoke.declaringClass.toJava
        val method_name = invoke.name
        if (class_name.equals(obj.className))
          obj.modifiedObject = "this." + method_name
        else {
          obj.modifiedObject = class_name.split('.').last + "." + method_name
        }
      case INVOKEVIRTUAL.opcode |
           INVOKESPECIAL.opcode |
           INVOKEINTERFACE.opcode =>
        var stringObject = new StringBuilder()
        objectToString(origin, stringObject, obj, byteCodeInfo, body, result)
        if (obj.isValid)
          obj.modifiedObject = stringObject.toString()
      case _ =>
        println("test")
    }
  }

  def objectToString(pc: Int, valueObject: StringBuilder, obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, body: Code,
                     result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    if (pc < 0) {
      if (pc == -1 && !obj.method.isStatic) {
        valueObject.insert(0, "this")
      } else
        obj.isValid = false
      return
    }
    val operands = result.operandsArray(pc)
    val instruction = body.instructions(pc)
    if (operands.equals(Naught) || operands.head.equals(operands.last) ) {
      instruction.opcode match {
        case GETFIELD.opcode =>
          val get_field = instruction.asInstanceOf[GETFIELD]
          valueObject.insert(0, "this." + get_field.name)
        case GETSTATIC.opcode =>
          val get_static = instruction.asInstanceOf[GETSTATIC]
          valueObject.insert(0, "this." + get_static.name)
        case NEW.opcode =>
          val instruction_new = instruction.asInstanceOf[NEW]
          valueObject.insert(0, "new " + instruction_new.objectType.simpleName + "()")
        case INVOKESTATIC.opcode =>
          val invokeStatic = instruction.asInstanceOf[INVOKESTATIC]
          val className = invokeStatic.declaringClass.toJava
          val methodName = invokeStatic.name
          if (className.equals(obj.className))
            valueObject.insert(0, "this." + methodName + "()")
          else {
            valueObject.insert(0, className.split('.').last + "." + methodName + "()")
          }
        case INVOKEVIRTUAL.opcode | INVOKESPECIAL.opcode | INVOKEINTERFACE.opcode =>
          val name = instruction.asInvocationInstruction.name
          valueObject.insert(0, "." + name + "()")
          result.domain.originsIterator(operands.last).foreach(org =>
            objectToString(org, valueObject, obj, byteCodeInfo, body, result)
          )
        case _ =>
      }
    } else {
      val paramString = new StringBuilder()
      var new_info = new ByteCodeInfo()
      operands.foreach(op => {
        if (instruction.isInstanceOf[INVOKESTATIC] | !op.equals(operands.last)) {
          op match {
            case result.domain.StringValue(s) =>
              createParamString(paramString, s, operands.head.equals(op))
            case result.domain.MultipleReferenceValues(v) =>
              obj.isValid = false
              return
            case result.domain.ClassValue(v) =>
              createParamString(paramString, v.asObjectType.simpleName + ".class", operands.head.equals(op))
            case result.domain.DomainReferenceValueTag(v) =>
              result.domain.originsIterator(op).foreach(org =>
                if (org == -1 && !obj.method.isStatic) {
                  createParamString(paramString, "this", operands.head.equals(op))
                } else {
                  obj.isValid = false
                  return
                }
              )
            case _ =>
              obj.isValid = false
              return
          }
        } else {
          paramString.update(0, '(')
          instruction.opcode match {
            case INVOKEVIRTUAL.opcode | INVOKESPECIAL.opcode | INVOKEINTERFACE.opcode =>
              val invoke = instruction.asInvocationInstruction
              paramString.insert(0, "." + invoke.name)
              valueObject.insert(0, paramString)
              result.domain.originsIterator(op).foreach(org =>
                objectToString(org, valueObject, obj, byteCodeInfo, body, result)
              )
            case _ =>
              println("test")
          }
        }
      })
      if (instruction.isInstanceOf[INVOKESTATIC]) {
        paramString.update(0, '(')
        val invokeStatic = instruction.asInstanceOf[INVOKESTATIC]
        val className = invokeStatic.declaringClass.toJava
        val methodName = invokeStatic.name
        if (className.equals(obj.className)) {
          paramString.insert(0, "this." + methodName)
        } else {
          paramString.insert(0, className.split('.').last + "." + methodName)
        }
        valueObject.insert(0, paramString)
      }
    }
  }

  def createParamString(paramString: StringBuilder, str: String, isHead: Boolean): Unit = {
    if (isHead) {
      paramString.append("," + str + ")")
    } else {
      paramString.insert(0, "," + str)
    }
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
    operands.foreach(op => {
      if (!obj.isValid) return
      if (!op.equals(operands.last)) {
        op match {
          case result.domain.DomainInitializedArrayValueTag(v) =>
            val array_length = v.length.get
            if (array_length > 0) {
              var param_list = new ListBuffer[ByteCodeInfo]()
              byteCodeInfo.parametersByteCodeInfo = Option(param_list)
              getTypeParametersOfMethod(v.origin, origin, array_length, classConstructor, byteCodeInfo, body)
            }
          case result.domain.MultipleReferenceValues(v) =>
            obj.isValid = false
            return
          case result.domain.DomainArrayValueTag(v) =>
            v.originsIterator.foreach(org => {
              if (org < 0) {
                obj.isValid = false
                return
              }
              val instr = body.instructions(org)
              instr.opcode match {
                case GETSTATIC.opcode | GETFIELD.opcode =>
                  obj.isValid = false
                  return
                case _ =>
              }
            })
          case _ =>
        }
      }
    })

    if (!obj.isValid) return
    op = operands.last
    op match {
      case result.domain.MultipleReferenceValues(v) =>
        obj.isValid = false
        return
      case _ =>
        result.domain.originsIterator(op).foreach(org => {
          getClassInfos(org, classNames, obj, new_info, body, result)
          if (!obj.isValid) return
        })
    }
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    if (obj.isValid) {
      classConstructor.className = classNames.last
      obj.classConstructor = Option(classConstructor)
    }
  }

  def setMethodToObject(origin: Integer, obj: ReflectionUse, byteCodeInfo: ByteCodeInfo, body: Code, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
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
      case INVOKEVIRTUAL.opcode |
           INVOKESPECIAL.opcode |
           INVOKEINTERFACE.opcode =>
        val operands = result.operandsArray(origin)
        if (operands == null) {
          return
        }
        operands.foreach( op =>
        {
          if (op.equals(operands.last)) {
            result.domain.originsIterator(op).foreach(org => {
              getClassInfos(org, classNames, obj, new_info, body, result)
              if (!obj.isValid) return
            })
          } else {
            op match {
              case result.domain.StringValue(s) =>
                var lst = new ListBuffer[ByteCodeInfo]()
                var paramInfo = new ByteCodeInfo
                val org = result.domain.origins(op).head
                paramInfo.setInfo(org, body.instructions(org))
                methodAndClass.methodName = s
                lst.append(paramInfo)
                byteCodeInfo.parametersByteCodeInfo = Option(lst)
              case result.domain.DomainInitializedArrayValueTag(v) =>
                val param_length = v.length.get
                if (param_length > 0) {
                  var lst = new ListBuffer[ByteCodeInfo]()
                  byteCodeInfo.parametersByteCodeInfo = Option(lst)
                  getTypeParametersOfMethod(v.origin, origin, v.length.get, methodAndClass, byteCodeInfo, body)
                }
              case _ =>
                obj.isValid = false
                return
            }
          }
        })
      case _ =>
        obj.isValid = false
        return
    }
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    if (obj.isValid) {
      methodAndClass.className = classNames.last
      obj.methodAndClass = Option(methodAndClass)
    }
  }

  def getValuesOfParameters(start: Int, end: Int, obj: ReflectionUse, info_list: ListBuffer[String] ,body: Code,
                            result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}): Unit = {
    val prePc = body.pcOfPreviousInstruction(end)
    if (prePc == start)
      return
    val instruction = body.instructions(prePc)
    instruction match {
      case AASTORE =>
        var new_info = new ByteCodeInfo()
        /*var info = new StringBuilder()
        getParameterInReflectionFunction(body.pcOfPreviousInstruction(prePc), body, obj, new_info, info, result)
        obj.asInstanceOf[ReflectionInvoke].valueInfo.append(new_info)
        info_list.append(info.toString())*/
      case _ =>
        getValuesOfParameters(start, prePc, obj, info_list, body, result)
    }
  }

  def getTypeParametersOfMethod(start: Integer, end: Integer, count: Integer, obj: ObjectCalledByReflection, byteCodeInfo: ByteCodeInfo, body: Code): Unit = {
    val nextPc = body.pcOfNextInstruction(start)
    if (count == 0 || nextPc == end)
      return
    var new_count = count
    val instruction = body.instructions(nextPc)
    instruction match {
      case loadClass: LoadClass =>
        if (loadClass.value.isArrayType){
          saveParametersInfoInByteCodeInfo(nextPc, loadClass.value.toJava, instruction, obj, byteCodeInfo)
        } else {
          if (loadClass.value.asObjectType.id == ObjectType.Class.id) {
            return
          }
          val paramType = loadClass.value.asObjectType.fqn
          saveParametersInfoInByteCodeInfo(nextPc, paramType, instruction, obj, byteCodeInfo)
        }
        new_count = new_count - 1
      case getStatic: GETSTATIC =>
        val paramType = getStatic.declaringClass.fqn
        saveParametersInfoInByteCodeInfo(nextPc, paramType, instruction, obj, byteCodeInfo)
        new_count = new_count - 1
      case _ =>
    }
    getTypeParametersOfMethod(nextPc, end, new_count, obj, byteCodeInfo, body)
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
    if (operands == null || operands.equals(Naught)) {
      obj.isValid = false
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
      case result.domain.MultipleReferenceValues(v) =>
        obj.isValid = false
        return
      case op@result.domain.DomainReferenceValueTag(v) =>
        if (op.equals(operands.last)) {
          result.domain.originsIterator(op).foreach(org => {
            if (fieldAndClass.fieldName == "") return
            getClassInfos(org, classNames, obj, new_info, body, result)
          })
        } else
          result.domain.originsIterator(op).foreach(org => {
            setFieldToObject(org, obj, new_info, body, result)
          })
      case _ =>
    }
    byteCodeInfo.previousByteCodeInfo = Option(new_info)
    if (obj.isValid) {
      fieldAndClass.className = classNames.last
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
      case _: GETSTATIC =>
        obj.isValid = false
        return
      case _: GETFIELD =>
        obj.isValid = false
        return
      case invokeVirtual: INVOKEVIRTUAL =>
        if (operands.last.equals(operands.last) && (invokeVirtual.name == "get")) {
          operands.head match {
            case result.domain.MultipleReferenceValues(v) =>
              obj.isValid = false
              return
            case result.domain.DomainReferenceValueTag(v) =>
              classNames.append(v.upperTypeBound.head.asInstanceOf[ObjectType].fqn)
            case _ =>
          }
        }
      case invokeStatic: INVOKESTATIC =>
        val name = invokeStatic.name
        if (name == "getClass" && invokeStatic.methodDescriptor.parameterTypes.length > 0){
          obj.isValid = false
          return
        }
      case _ =>
    }
    instruction.opcode match {
      case INVOKEVIRTUAL.opcode |
           INVOKESPECIAL.opcode |
           INVOKESTATIC.opcode |
           INVOKEINTERFACE.opcode =>
        val methodName = instruction.asInvocationInstruction.name
        if (methodName == "getSuperclass" || methodName == "asSubclass") {
          if (!operands.head.equals(operands.last)) {
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
              getAnotherInfos(org, new_info, body, result)
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
        }
        else {
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
                  obj.isValid = false
                  return
                case result.domain.DomainReferenceValueTag(v) =>
                  lst.append(param_info)
                  result.domain.originsIterator(op).foreach(org => {
                    if (org <0) return
                    val instr = body.instructions(org)
                    if (instr.isInvocationInstruction && instr.asInvocationInstruction.name == "getName"){
                      getClassInfos(org, classNames, obj, param_info, body, result)
                    } else
                      getAnotherInfos(org, param_info, body, result)
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
                  getAnotherInfos(org, new_info, body, result)
                })
              }
            }

          })
        }
      case _ =>
    }
  }

  def getAnotherInfos(org: Int, byteCodeInfo: ByteCodeInfo, body: Code,
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
            return
          case result.domain.DomainReferenceValueTag(v) =>
            result.domain.originsIterator(op).foreach(org => {
              getAnotherInfos(org, param_info, body, result)
            })
          case _ =>
        }
      } else {
        var new_info = new ByteCodeInfo()
        byteCodeInfo.previousByteCodeInfo = Option(new_info)
        result.domain.originsIterator(op).foreach(org => {
          getAnotherInfos(org, new_info, body, result)
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
    info.equals("java.lang.reflect.Method") || info.equals("java.lang.Class") ||
      info.equals("java.lang.reflect.Field") || info.equals("java.lang.reflect.Construction")
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

  def main(args: Array[String]): Unit = {
    val methods_with_body = project.allMethodsWithBody
    val method_using_reflection = methods_with_body.filter(method => isMethodUsingReflection(method))
    methods_with_body.foreach(method => {
      setMethodCallReflection(method)
    })
    println("invoke: " + methodCallReflectionInvoke.length)
    println("setAccessible: " + methodCallReflectionSetAccessible.length)
    println("set: " + methodCallReflectionSet.length)

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
