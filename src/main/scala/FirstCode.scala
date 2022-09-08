import javassist._
import org.opalj.ai.{AIResult, ValuesDomain}
import org.opalj.ai.domain.{PerformAI, RefineDefUseUsingOrigins}
import org.opalj.ai.domain.l1.DefaultDomainWithCFGAndDefUse
import org.opalj.br.Code.unapply
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, Instruction, LoadClass_W}
import org.opalj.br._
import org.opalj.collection.immutable.Naught

import java.net.URL
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.break


object FirstCode {

  val projectJAR = "C:/Users/tam20/java_bytecode_manipulation/project/project/target/lib/cinemark-dex2jar.jar"
  val project: Project[URL] = Project(new java.io.File(projectJAR))
  val reflectPackageName = "java.lang.reflect"
  var methodCallReflectionInvoke: ListBuffer[Method] = new ListBuffer[Method]()
  var methodCallReflectionSet: ListBuffer[Method] = new ListBuffer[Method]()
  var methodCallReflectionSetAccessible: ListBuffer[Method] = new ListBuffer[Method]()
  var methodCallReflectionTrySetAccessible: ListBuffer[Method] = new ListBuffer[Method]()

  def setMethodCallReflection(method: Method): Unit = {
    if (method.body.isDefined) {
      val body = method.body.get
      val domain = new DefaultDomainWithCFGAndDefUse(project, method) with RefineDefUseUsingOrigins
      val result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]} = PerformAI(domain)
      body.iterate{ (pc, instr) =>
        if (instr.isInvocationInstruction){
          val name = instr.asInvocationInstruction.name
          val invoke = instr.asMethodInvocationInstruction
          if (checkInstructionCallReflection(instr)) {
            if (name.equals("invoke")) {
              methodCallReflectionInvoke += method
            }
            else if (name.equals("setAccessible")) {
              methodCallReflectionSetAccessible += method
              var obj = new ReflectionUse()
              getInfosFormSetAccessible(pc, result, invoke.methodDescriptor.parameterTypes.size, body, obj)

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

      /*case result.domain.MultipleReferenceValues(s) ⇒ s.foreach {
        case result.domain.StringValue(st) ⇒ println("st: " + st)
        case value ⇒ value.origins.foreach(getInfosFormSetAccessible(_, result, parameters_size, body))
      }*/
      case e ⇒
        println(e)
    }

  }

  def setFieldToObject(origin: Integer, obj: ReflectionUse, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}, body: Code): Unit = {
    val operands = result.operandsArray(origin)
    if (operands == null) {
      return
    }
    operands.foreach {
      case result.domain.StringValue(s) => obj.setAttributeName(s)
      case op@result.domain.DomainReferenceValueTag(v) =>
        if (v.allValues.exists(p =>
          p.upperTypeBound.containsId(ObjectType.Class.id))){
          result.domain.originsIterator(op).foreach(org => {
            setClassToObject(org, obj, result, body)
          })
        }
      case _ =>
    }
  }

  def setClassToObject(origin: Integer, obj: ReflectionUse, result: AIResult {val domain: DefaultDomainWithCFGAndDefUse[URL]}, body: Code): Unit = {
    val operands = result.operandsArray(origin)
    if (operands == null)
      return
    if (operands.equals(Naught)){
      val instruction = body.instructions(origin)
      instruction match {
        case load_class: LoadClass_W =>
          val value = load_class.value
          obj.setClassName(value.asObjectType.simpleName)
        case _ =>
      }
    } else {
      operands.foreach{
        case result.domain.StringValue(s) => obj.setClassName(s)
        case _ =>
      }
    }
  }


  def checkInstructionCallReflection(instruction: Instruction): Boolean ={
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

  def checkReflection(info: String):Boolean = {
    info.startsWith(reflectPackageName) || info.equals("java.lang.Class")
  }

  def isMethodUsingReflection(method: Method): Boolean ={
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

  def workWithReflectionSetAccessible(method: Method): Unit ={
    val domain = new DefaultDomainWithCFGAndDefUse(project, method) with RefineDefUseUsingOrigins
    val body = method.body.get
    lazy val  result: AIResult{ val domain: DefaultDomainWithCFGAndDefUse[URL]} = PerformAI(domain)
    body.iterate{ (pc, instruction) =>
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
