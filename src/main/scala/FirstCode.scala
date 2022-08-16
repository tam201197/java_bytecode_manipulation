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

  def checkInstructionCallReflection(instructions: Array[Instruction]): Boolean ={
    for (instr <- instructions) {
      instr match {
        case invokevirtual: INVOKEVIRTUAL =>
          if (checkReflection(invokevirtual.declaringClass.toJava))
            return true
        case invokestatic: INVOKESTATIC =>
          if (checkReflection(invokestatic.declaringClass.toJava))
          return true
        case invokeinterface: INVOKEINTERFACE =>
          if (checkReflection(invokeinterface.declaringClass.toJava))
            return true
        case invokespecial: INVOKESPECIAL =>
          if (checkReflection(invokespecial.declaringClass.toJava))
            return true
        case _ =>
      }
    }
    false
  }

  def checkReflection(info: String):Boolean = {
    info.startsWith(reflectPackageName) || info.equals("java.lang.Class")
  }

  def isMethodUsingReflection(method: Method): Boolean ={
    if (method.body.isDefined) {
      val code = method.body.get
      val i_invoke = code.instructions.filter(instr => instr != null && instr.isInvocationInstruction)
      if (checkInstructionCallReflection(i_invoke)) {
        return true
      }
      false
    }
    else false
  }

  def main(args: Array[String]): Unit = {
    val methods_with_body = p.allMethodsWithBody
    val method_using_reflection = methods_with_body.filter(method => isMethodUsingReflection(method))
    println(method_using_reflection.length)
    var count = 0
    val pool = ClassPool.getDefault
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
    println(count)
  }
}
