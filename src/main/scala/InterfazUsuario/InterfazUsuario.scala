package InterfazUsuario

import scala.io._
import Alimentos._
import InsumosPlasticos._
import scala.util._

object InterfazUsuario extends App
{
    var cerrarTienda : Boolean = false
    private var caja : Caja = new Caja
    private var tipoGrande : TipoTamano = new TipoTamano()
    tipoGrande.descripcion = "Grande"
    caja.agregarTamano(tipoGrande)

    while(!cerrarTienda)
    {
        println("Bienvenido a la configuración de comidas rápidas")
        println("Sus opciones son: \n 1->Crear Alimento \n 2->MostrarCatalogo \n 3->Salir")
        println("Escoja una opción: ")
        var opcion : Int = StdIn.readInt()
        if (opcion == 1)
        {
            var resultadoOperacion = crearAlimentoCatalogo()
            resultadoOperacion match
            {
                case Success(s) => println("Producto Creado Correctamente")
                case Failure(f) => println(f)
            }    
        }
        if (opcion == 2)
        {
            var probarCatalogo = mostrarComida()
            probarCatalogo match
            {
                case Success(s) => caja.mostrarCatalogo().foreach(p => println(p.descripcion + " " + p.referencia))
                case Failure(f) => println(f) 
            }
            
        }
        if (opcion == 3)
        {
            println("Cerrando sistema.")
            cerrarTienda = true
        }
    }

    def mostrarComida() : Try[Unit] =
    {
        return Try{
            if(caja.mostrarCatalogo.isEmpty)
            {
                throw new Exception("El catalogo está vacío\n")
            }
        }
    }
    def crearAlimentoCatalogo() : Try[Unit] = 
    {
        return Try{
            println("Qué alimento desea crear? Hamburguesa, Bebida o Papas");
            var opcionComida : String = StdIn.readLine()
            var claseComida = Class.forName("Alimentos." + opcionComida)
            var instanciaComida = claseComida.newInstance();
            var comida = instanciaComida.asInstanceOf[Alimento]
            println("Coloque el nombre de su " + opcionComida)
            var nombre : String = StdIn.readLine()
            println("Coloque el precio de su " + opcionComida)
            var precio : Double = StdIn.readDouble()
            comida.descripcion = nombre
            comida.setCosto(precio)
            var tipoTamano : Option[TipoTamano] = elegirTamano()
            if (tipoTamano.isEmpty)
            {
                throw new Exception("Lista de tamaños Vacía")
            }
            comida.tamano = tipoTamano.get
            caja.agregarAlimentoCatalogo(comida)
        }
    }

    def elegirTamano() : Option[TipoTamano] = 
    {
        var tamanos : List[TipoTamano] = caja.mostrarTamanos()
        println("Tamaños actuales: ")
        tamanos.foreach{ t => 
        {
            println("Referencia : " + t.idTipoTamano + " Descripción: " + t.descripcion)
        }}
        println("Escriba la referecia de su tamaño: ")
        var referencia : String = StdIn.readLine()
        var tipoTamanoElejido : Option[TipoTamano] = tamanos.filter(t => t.idTipoTamano == referencia).headOption
        return tipoTamanoElejido
    }

}