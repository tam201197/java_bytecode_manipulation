import javassist._
import org.opalj.ai.{AIResult, ValuesDomain}
import org.opalj.ai.domain.{PerformAI, RefineDefUseUsingOrigins}
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.br.Code.unapply
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
      val body = method.body.get
      val domain = new DefaultDomainWithCFGAndDefUse(project, method) with RefineDefUseUsingOrigins
      val result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]} = PerformAI(domain)
      body.iterate { (pc, instr) =>
        if (instr.isInvocationInstruction) {
          val name = instr.asInvocationInstruction.name
          val invoke = instr.asMethodInvocationInstruction
          if (checkInstructionCallReflection(instr)) {
            if (name.equals("invoke")) {
              methodCallReflectionInvoke += method
            }
            else if (name.equals("setAccessible")) {
              methodCallReflectionSetAccessible += method
              val obj = new ReflectionUse()
              getInfosFormSetAccessible(pc, result, invoke.methodDescriptor.parameterTypes.size, body, obj)
              setAccessibleObjects += obj
              val len = setAccessibleObjects.length
              if (len == 63)
                println("test " + len)

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

  def getInfosFormSetAccessible(pc: Integer, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}, parameters_size: Integer, body: Code, obj: ReflectionUse): Unit = {
    val operands = result.operandsArray(pc)
    if (operands == null) {
      return
    }
    operands.foreach {
      case result.domain.StringValue(s) =>
      case op@result.domain.DomainReferenceValueTag(v) =>
        if (v.allValues.exists(p =>
          p.upperTypeBound.containsId(ObjectType("java/lang/reflect/Field").id))) {
          result.domain.originsIterator(op).foreach(origin => {
            setFieldToObject(origin, obj, result, body)
          })
        }
        else if (v.allValues.exists(p =>
          p.upperTypeBound.containsId(ObjectType("java/lang/reflect/Method").id))) {
          result.domain.originsIterator(op).foreach(origin => {
            setMethodToObject(origin, obj, result, body)
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

  def setMethodToObject(origin: Integer, obj: ReflectionUse, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}, body: Code): Unit = {
    if (origin < 0) return
    val instruction = body.instructions(origin)
    var classNames = new ListBuffer[String]()
    instruction.opcode match {
      case GETFIELD.opcode =>
        val invoke = instruction.asInstanceOf[GETFIELD]
        obj.setMethodName(invoke.name)
        obj.setClassName(invoke.declaringClass.fqn)
      case GETSTATIC.opcode =>
        val invoke = instruction.asInstanceOf[GETSTATIC]
        obj.setMethodName(invoke.name)
        obj.setClassName(invoke.declaringClass.fqn)
      case INVOKEVIRTUAL.opcode |
           INVOKESPECIAL.opcode |
           INVOKESTATIC.opcode |
           INVOKEINTERFACE.opcode =>
        val operands = result.operandsArray(origin)
        if (operands == null) {
          return
        }
        operands.foreach {
          case result.domain.StringValue(s) => obj.setMethodName(s)
          case op@result.domain.DomainInitializedArrayValueTag(v) =>
            getMethodParameters(v.origin, origin, v.length.get, obj, body)
          case op@result.domain.DomainReferenceValueTag(v) =>
            /*if (v.allValues.exists(p =>
              p.upperTypeBound.containsId(ObjectType.Class.id))) {
              result.domain.originsIterator(op).foreach(org => {
                getClassInfos(org, classNames, result, body)
              })*/
//            }
          case op@_ =>
            result.domain.originsIterator(op).foreach(org => {
              setMethodToObject(org, obj, result, body)
            })
        }
    }
  }

  def getMethodParameters(start: Integer, end: Integer, count: Integer, obj: ReflectionUse, body: Code ): Unit = {
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

  def setFieldToObject(origin: Integer, obj: ReflectionUse, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}, body: Code): Unit = {
    if (origin < 0) return
    val operands = result.operandsArray(origin)
    if (operands == null) {
      return
    }
    var attributeName = ""
    val classNames = new ListBuffer[String]()
    operands.foreach {
      case result.domain.StringValue(s) => {
        if (s == null){
          val instruction = body.instructions(origin)
          instruction match {
            case loadStringW: LoadString_W =>
              attributeName = loadStringW.value
            case _ =>
          }
        } else
          attributeName = s
      }
      case op@result.domain.DomainReferenceValueTag(v) =>
        v.allValues.foreach(p => {
          if (p.upperTypeBound.containsId(ObjectType.String.id))
            result.domain.originsIterator(op).foreach(org =>{
              setFieldToObject(org, obj, result, body)
              if (obj.getAttribute().isEmpty)
                return
            })
          if (p.upperTypeBound.containsId(ObjectType.Class.id)) {
            result.domain.originsIterator(op).foreach(org => {
              if (org < 0) return
              getClassInfos(org, classNames, result, body)
            })
          }
        })
      case _ =>
    }
    obj.setAttributeName(attributeName, classNames)
  }

  def getClassInfos(origin: Integer, classNames: ListBuffer[String], result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}, body: Code): Unit = {
    if (origin < 0) return
    val operands = result.operandsArray(origin)
    if (operands == null) return
    if (operands.equals(Naught)) {
      val instruction = body.instructions(origin)
      instruction match {
        case loadClassW: LoadClass_W =>
          val value = loadClassW.value
          classNames.append(value.asObjectType.fqn)
        case loadClass: LoadClass =>
          val value = loadClass.value
          classNames.append(value.asObjectType.fqn)
        case loadString: LoadString =>
          classNames.append(loadString.value)
        case _ =>
      }
    } else {
      operands.foreach {
        case result.domain.StringValue(s) => classNames.append(s)
        case op@result.domain.DomainReferenceValueTag(v) =>
          result.domain.originsIterator(op).foreach(org => {
            if (org < 0){
              val upperType = v.leastUpperType
              if (upperType != null && upperType.get.asObjectType.fqn != "java/lang/Object"){
                classNames.append(upperType.get.asObjectType.fqn)
              }
            } else
              getClassInfos(org, classNames, result, body)
          })
        case _ =>
      }
    }
    if (classNames.length == 0){
      val instruction = body.instructions(origin)
      instruction.opcode match {
        case INVOKEVIRTUAL.opcode |
             INVOKESPECIAL.opcode |
             INVOKESTATIC.opcode |
             INVOKEINTERFACE.opcode =>
          if (instruction.asMethodInvocationInstruction.name == "getSuperclass"){
            classNames.append(result.domain.method.classFile.superclassType.get.fqn)
          }
        case _ => return
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
