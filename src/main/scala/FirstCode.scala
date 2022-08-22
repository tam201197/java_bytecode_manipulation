import javassist._
import org.opalj.br.analyses.Project
import org.opalj.br.instructions.{INVOKEDYNAMIC, INVOKEINTERFACE, INVOKESPECIAL, INVOKESTATIC, INVOKEVIRTUAL, Instruction}
import org.opalj.br._

import java.net.URL
import scala.util.control.Breaks.break


object FirstCode {

  val projectJAR = "C:/Users/tam20/java_bytecode_manipulation/project/project/target/lib/cinemark-dex2jar.jar"
  val p: Project[URL] = Project(new java.io.File(projectJAR))
  val reflectPackageName = "java.lang.reflect"
  var methodCallReflectionInvoke: Array[Method] = Array()
  var methodCallReflectionSet: Array[Method] = Array()
  var methodCallReflectionSetAccessible: Array[Method] = Array()

  def setMethodCallReflection(method: Method): Unit = {
    if (method.body.isDefined) {
      val code = method.body.get
      val instructions = code.instructions.filter(instr => instr != null && instr.isInvocationInstruction)
      var name = ""
      for (instr <- instructions){
        name = instr.asInvocationInstruction.name
        instr.asInvocationInstruction
        if (name.equals("invoke"))
          methodCallReflectionInvoke:+ method
        else if (name.equals("setAccessible"))
          methodCallReflectionSetAccessible:+ method
        else if (name.startsWith("set") && checkInstructionCallReflection(instr))
          methodCallReflectionSet:+ method
      }
    }
  }

  def checkInstructionCallReflection(instruction: Instruction): Boolean ={
    instruction match {
      case invokevirtual: INVOKEVIRTUAL =>
        checkReflection(invokevirtual.declaringClass.toJava)
      case invokestatic: INVOKESTATIC =>
        checkReflection(invokestatic.declaringClass.toJava)
      case invokeinterface: INVOKEINTERFACE =>
        checkReflection(invokeinterface.declaringClass.toJava)
      case invokespecial: INVOKESPECIAL =>
        checkReflection(invokespecial.declaringClass.toJava)
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

  def main(args: Array[String]): Unit = {
    val methods_with_body = p.allMethodsWithBody
    val method_using_reflection = methods_with_body.filter(method => isMethodUsingReflection(method))
    println(method_using_reflection.length)
    methods_with_body.foreach(method => setMethodCallReflection(method))
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
